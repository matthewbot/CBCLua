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
		local prefix
		
		if mod == nil then
			mod = arg
			prefix = "std"
		else
			prefix = arg
		end
		
		cbcluamodule(mod)
		table.insert(mod._AUTOREQS, prefix)
	end
end

-- Module modifier that puts the module in KISS-C compat mode

function kissc_compat(mod) 
	cbcluamodule(mod)
	table.insert(mod._IMPORTS, require("std.kissc"))
	table.insert(mod._IMPORTS, require("raw.cbc"))
end

-- This function loads a module, then makes it visible in the current module

function import(name)
	local outermod = getfenv(2)	
	assert(outermod._NAME, "import may only be called from within a module!")

	table.insert(outermod._IMPORTS, require(name))
end

function submodule(name)
	local outermod = getfenv(2)
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
		return _M
	end
	
	for _,importmod in ipairs(mod._IMPORTS) do -- look through its imports to find the value
		local val = importmod[key]
		if val then
			return val
		end
	end
	
	for _,autoreqprefix in ipairs(mod._AUTOREQS) do
		local modname = autoreqprefix .. "." .. key
		local ok, automod = pcall(require, modname)
		if ok then
			return automod
		end
	end
	
	local val = _G[key] -- otherwise, go through global functions
	if type(val) == "function" then
		return val
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
	
	error("Undefined global '" .. key .. "'", 2)
end
