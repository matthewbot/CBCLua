-- Some expansions on the module system

local mod_mt = { }

-- Module modifier that allows import() to work and requires declaration of global variables through the global() function

function cbcluamodule(mod)
	if mod._CBCLUAMODULE then return end
	
	mod._IMPORTS = { }
	mod._SUBMODS = { }
	mod._GLOBALS = { }
	mod._CBCLUAMODULE = true
	setmetatable(mod, mod_mt)
end

-- Module modifier that automatically declares globals on assignment

function autoglobals(mod)
	cbcluamodule(mod)
	rawset(mod, "_AUTOGLOBALS", true)
end

-- Module modifier that puts the module in KISS-C compat mode

function kissc_compat(mod) 
	table.insert(mod._IMPORTS, require("raw.cbc"))
	table.insert(mod._IMPORTS, require("std.kissc"))
end

-- This function loads a module, then makes it visible in the current module

function import(name)
	local outermod = getfenv(2)	
	assert(outermod._NAME, "import may only be called from within a module!")

	table.insert(outermod._IMPORTS, require(name))
end

function submodule(name)
	local outermod = getfnev(2)
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
				error("Bad key in global declaration table of type " .. t)
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
	local funcs
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
	
	for _,importmod in ipairs(mod._IMPORTS) do -- look through its imports to find the value
		local val = importmod[key]
		if val then
			return val
		end
	end
	
	local val = _G[key] -- otherwise, go through global functions
	if type(val) == "function" then
		return val
	end
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
	
	error("Assignment to undefined global variable '" .. key .. "'", 2)
end
