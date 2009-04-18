-- Some expansions on the module system

local mod_mt = { }

-- Module modifier that allows import() to work and requires declaration of global variables through the global() function

function cbcluamodule(mod)
	mod._IMPORTS = { }
	mod._GLOBALS = { }
	setmetatable(mod, mod_mt)
end

-- Module modifier that automatically declares globals on assignment

function autoglobals(mod)
	mod._AUTOGLOBALS = true
end

-- Module modifier that puts the module in KISS-C compat mode

function kissc_compat(mod) 
	table.insert(mod._IMPORTS, require("raw.cbc"))
	table.insert(mod._IMPORTS, require("std.kissc"))
end

-- This function loads a module, then makes it visible in the current module

function import(name, depth)
	depth = depth or 1
	local outermod = getfenv(depth+1)	
	assert(outermod._NAME, "import may only be called from within a module!")

	table.insert(outermod._IMPORTS, require(name))
end

-- This function declares a global

function global(name, depth)
	depth = depth or 2
	local outermod = getfenv(depth)
	assert(outermod._NAME, "global may only be called from within a module!")
	
	local tname = type(name)
	
	if tname == "string" then
		outermod._GLOBALS[name] = true
	elseif tname == "table" then
		for _,v in pairs(name) do
			outermod._GLOBALS[v] = true
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
	for _,importmod in ipairs(mod._IMPORTS) do
		local val = importmod[key]
		if val then
			return val
		end
	end
	
	local val = _G[key]
	if type(val) == "function" then
		return val
	end
end

-- This function catches access to undefined globals

function mod_mt.__newindex(mod, key, value)
	if mod._AUTOGLOBALS or type(value) == "function" or mod._GLOBALS[key] then
		rawset(mod, key, value)
		return
	end
	
	error("Assignment to undefined global variable '" .. key .. "'", 2)
end
