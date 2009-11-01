-- Some expansions on the module system

local mod_mt = { }

-- Module modifier that allows import() to work and requires declaration of global variables through the global() function

function cbcluamodule(mod)
	if mod._CBCLUAMODULE then return end
	
	mod._M = nil
	mod._IMPORTS = { }
	mod._SUBMODS = { }
	mod._GLOBALS = { }
	mod._AUTOREQS = { }
	mod._CBCLUAMODULE = true
	setmetatable(mod, mod_mt)
end

-- Module modifier that automatically declares globals on assignment

function autoglobals(mod)
	cbcluamodule(mod)
	rawset(mod, "_AUTOGLOBALS", true)
end

-- Automatically require packages beginning with the prefix (defaults to std)

function autorequire(arg, mod)
	if mod == nil and type(arg) == "string" then -- just given a string
		return function (mod) -- make a closure for module() to call
			cbcluamodule(mod)
			table.insert(mod._AUTOREQS, arg)
		end
	else
		cbcluamodule(mod)
		table.insert(mod._AUTOREQS, arg)
	end
end

-- This function loads a module, then makes it visible in the current module
-- from the outside however, the imported module is not visible

function import(name, mod)
	local outermod = mod or getfenv(2)	
	assert(outermod._NAME, "import may only be called from within a module!")

	table.insert(outermod._IMPORTS, require(name))
end

-- This function loads a module, then makes it visible in the current module
-- the module is also visible from the outside

function submodule(name, mod)
	local outermod = mod or getfenv(2)
	assert(outermod._NAME, "submodule may only be called from within a module!")
	
	table.insert(outermod._SUBMODS, require(name))
end

-- This function declares a global
---- Usage: global("ultimate_answer", 42)
---- Or: global{ ultimate_answer = 42, ultimate_question = "Cannot be known in this universe" }

function global(name, val)
	local outermod = getfenv(2)
	assert(outermod._NAME, "global may only be called from within a module!")
	
	local tname = type(name)
	
	if tname == "string" then
		outermod._GLOBALS[name] = true
		if val ~= nil then
			rawset(outermod, name, val)
		end
	elseif tname == "table" then
		for k,v in pairs(name) do
			local t = type(k)
			
			if t == "number" then
				outermod._GLOBALS[v] = true
			elseif t == "string" then
				outermod._GLOBALS[k] = true
				rawset(outermod, k, v)
			else
				error("Bad key in global declaration table (type " .. t .. ")")
			end
		end
	else
		error("Expected string or table for argument #1 to global", 2)
	end
end

-- This function replaces module() and makes it so that leaving off any module modifiers automatically
-- adds the cbcluamodule modifier. This simplifies cbclua code and does little to break compatibility
-- with other lua code because htey almost always either have their own modifier or use module.seeall

local realmodule = module
function module(name, ...)
	if select("#", ...) == 0 then 
		realmodule(name, cbcluamodule)
	else
		realmodule(name, ...)
	end
	
	setfenv(2, getfenv(1)) -- propogate environment set by realmodule to caller
end

-- This function is a metatable function that allows modules to view global functions and imported modules

function mod_mt.__index(mod, key)
	for _,submod in ipairs(mod._SUBMODS) do -- look in sub modules
		local val = submod[key]
		if val then
			return val
		end
	end

	if getfenv(2) ~= mod then -- if the environment of our caller isn't the module (IE, its not a function declared inside the module)
		return nil -- no special lookup rules, quit now
	end
	
	if key == '_G' then
		return _G
	end
	
	if key == '_M' then
		return mod
	end
	
	if key == 'super' then
		return super
	end
	
	if mod._GLOBALS[key] then -- the global is defined, but it doesn't exist
		return nil -- return nil
	end
	
	local val = _G[key] -- otherwise, go through global functions
	if type(val) == "function" then
		return val
	end	
	
	for _,importmod in ipairs(mod._IMPORTS) do -- look through its imports to find the value
		local val = importmod[key]
		if val then
			return val
		end
	end
	
	for _,autoreqprefix in ipairs(mod._AUTOREQS) do
		local modname = autoreqprefix .. key
		local ok, automod = pcall(require, modname)
		if ok then
			return automod
		end
	end
	
	error("Undefined global '" .. key .. "'", 2)
end

-- This function catches assignment to undefined globals

-- You don't need to declare
---- Capitalized globals (should be class names, usually only assigned to once)
---- Globals that will contain functions (again, usually only assinged once)

function mod_mt.__newindex(mod, key, value)
	local firstbyte = key:byte(1)

	if (firstbyte >= 65 and firstbyte <= 90) or mod._AUTOGLOBALS or type(value) == "function" or mod._GLOBALS[key] then
		rawset(mod, key, value)
		return
	end
	
	-- hackish thing below
	if type(value) == "table" and getfenv(2) == _G then -- if its a table being assigned in, and the caller's environment is global
		return -- ignore it, lua's module system is creating globals inside our tables
	end

	for _,submod in ipairs(mod._SUBMODS) do -- look in sub modules
		if submod._GLOBALS[key] then
			rawset(submod, key, value)
			return
		end
	end
	
	for _,importmod in ipairs(mod._IMPORTS) do -- look through its imports to find the value
		if importmod._GLOBALS[key] then
			rawset(importmod, key, value)
			return
		end
	end

	error("Undefined global '" .. key .. "'", 2)
end

-- This function is used to do delayed module loading
-- original use was in the startup code, log and task have dependencies on each other
-- since log wants to display the current task and task wants to log messages
-- log uses a require_later on task to break the cyclic dependency

local mock_mod_mt = { }

function require_later(name)
	local mock_mod = { name = name }
	setmetatable(mock_mod, mock_mod_mt)
	return mock_mod
end

local function get_mod(mock_mod)
	local mod = rawget(mock_mod, mod)
	
	if mod == nil then
		mod = require(mock_mod.name)
		rawset(mock_mod, "mod", mod)
	end
	
	return mod
end

function mock_mod_mt.__index(mock_mod, key)
	local mod = get_mod(mock_mod)
	return mod[key]
end

function mock_mod_mt.__newindex(mock_mod, key, val)
	local mod = get_mod(mock_mod)
	mod[key] = val
end
	
