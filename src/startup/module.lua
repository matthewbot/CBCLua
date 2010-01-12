-- Some expansions on the module system

local mod_mt = { }

-- Module modifier that allows import() / export() to work. Also allows access to global functions

function cbcluamodule(mod)
	if mod._CBCLUAMODULE then return end
	
	mod._M = nil
	mod._IMPORTS = { }
	mod._EXPORTS = { }
	mod._AUTOREQS = { }
	mod._CBCLUAMODULE = true
	setmetatable(mod, mod_mt)
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

-- This function makes a module part of the current module's public interface

function export(name, mod)
	local outermod = mod or getfenv(2)
	assert(outermod._NAME, "export may only be called from within a module!")
	
	table.insert(outermod._EXPORTS, require(name))
end

-- This function replaces module() and makes it so that leaving off any module modifiers automatically
-- adds the cbcluamodule modifier. Our changes shouldn't break any code operating under the expectations
-- of the real module function, since everything we don't alter any behavior, just add new ones

local realmodule = module
local _G = _G
function module(name, ...)
	if select("#", ...) == 0 and not name:find("^socket") then 
		realmodule(name, _G.cbcluamodule)
	else
		realmodule(name, ...)
	end
	
	_G.setfenv(2, _G.getfenv(1)) -- propogate environment set by realmodule to caller
end

-- This function is a metatable function that allows modules to view global functions and imported modules

function mod_mt.__index(mod, key)
	for _,exportmod in ipairs(mod._EXPORTS) do -- look in export modules
		local val = exportmod[key]
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
end

-- This allows assignments into exported modules and imports
-- it also hacks around lua's module system's behavior of adding tables for submodules in a module table

function mod_mt.__newindex(mod, key, value)	
	if type(value) == "table" and getfenv(2) == _G then -- if its a table being assigned in, and the caller's environment is global
		return -- ignore it, lua's module system is creating globals inside our tables
	end

	for _,exportmod in ipairs(mod._EXPORTS) do -- look in exported modules
		if exportmod[key] then -- if it exists in the exported module (this will not allow assignment to nil variables!)
			rawset(exportmod, key, value) -- set it
			return
		end
	end
	
	if getfenv(2) ~= mod then -- if the caller was declared inside the module
		for _,importmod in ipairs(mod._IMPORTS) do -- look through its imports to find the value
			if importmod[key] then
				rawset(importmod, key, value)
				return
			end
		end
	end
	
	rawset(mod, key, value)
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
	
