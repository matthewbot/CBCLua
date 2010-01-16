-- CBCLua Module system

-- Public API for CBCLua Modules --

local mod_mt = { }
local get_system_table

-- Automatically require packages beginning with the prefix

function autorequire(name, mod)
	mod = mod or getfenv(2)	
	assert(getmetatable(mod) == mod_mt, "autorequire may only be called from within a module!")

	local autoreqs = get_system_table(mod, "_AUTOREQS")
	table.insert(autoreqs, name)
end

-- This function loads a module, then makes it visible in the current module
-- from the outside however, the imported module is not visible

function import(name, mod)
	mod = mod or getfenv(2)	
	assert(getmetatable(mod) == mod_mt, "import may only be called from within a module!")

	local imports = get_system_table(mod, "_IMPORTS")
	table.insert(imports, require(name))
end

-- This function makes a module part of the current module's public interface

function export(name, mod)
	mod = mod or getfenv(2)
	assert(getmetatable(mod) == mod_mt, "export may only be called from within a module!")
	
	local exports = get_system_table(mod, "_EXPORTS")
	table.insert(exports, require(name))
end

function make_cbclua_module(table)
	setmetatable(table, mod_mt)
end
	
-- Internals

function get_system_table(mod, tabname)
	local tab = mod[tabname]
	if tab == nil then
		tab = { }
		mod[tabname] = tab
	end
	
	return tab
end
	
local function dummymodule()
end
	
-- Module metatable

function mod_mt.__index(mod, key)
	local exports = rawget(mod, "_EXPORTS")
	if exports then
		for _,exportmod in ipairs(exports) do -- look in export modules
			local val = exportmod[key]
			if val then
				return val
			end
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
	
	if key == 'module' then
		return dummymodule
	end
	
	local val = _G[key] -- otherwise, go through global functions
	if type(val) == "function" then
		return val
	end	
	
	local imports = rawget(mod, "_IMPORTS")
	if imports then
		for _,importmod in ipairs(imports) do -- look through its imports to find the value
			local val = importmod[key]
			if val then
				return val
			end
		end
	end
	
	local autoreqs = rawget(mod, "_AUTOREQS")
	if autoreqs then
		for _,autoreqprefix in ipairs(autoreqs) do -- figure out if we can autorequire it
			local modname = autoreqprefix .. key
			local ok, automod = pcall(require, modname)
			if ok then
				mod[key] = automod
				return automod
			end
		end
	end
end

local CBCLUA_CODEPATH = os.getenv("CBCLUA_CODEPATH")
local CBCLUA_MODSPATH = os.getenv("CBCLUA_MODSPATH")
	
local function tryload(fname)
	local f = io.open(fname)
	if f then
		f:close()
		return loadfile(fname)
	end
end
	
local function loader(name)
	local basename
	if name:match("^cbclua%.") then
		basename = CBCLUA_MODSPATH .. name:gsub("^cbclua%.", ""):gsub("%.", "/")
	else
		basename = CBCLUA_CODEPATH .. name:gsub("%.", "/")
	end
	
	local filename = basename .. ".lua"
	local modfilename = basename .. "/mod.lua"
	
	local modfunc = tryload(filename) or 
	                tryload(modfilename) 
	                
	if modfunc == nil then
		return nil
	end
	
	local mod = { _NAME = name }
	
	setmetatable(mod, mod_mt)
	setfenv(modfunc, mod)
	
	return function ()
		package.loaded[name] = mod -- this, for some reason, must happen inside the module function
		return modfunc() -- so we make a thunk wrapper around it
	end
end

table.insert(package.loaders, 1, loader)

