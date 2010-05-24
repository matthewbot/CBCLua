-- Public API for CBCLua Modules --

local mod_mt = { }
local all_codemod_names = { }
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

function export_partial(name, mod, ...)
	local names

	if type(mod) == "table" then
		names = {...}
	else
		names = {mod, ...}
		mod = getfenv(2)
	end
	
	assert(getmetatable(mod) == mod_mt, "export_partial may only be called from within a module!")	
	
	local partial_exports = get_system_table(mod, "_PARTIAL_EXPORTS")
	
	local names_set = { }
	for _, val in ipairs(names) do
		names_set[val] = true
	end
	table.insert(partial_exports, {mod = require(name), names = names_set})
end

function cbclua_make_module(table)
	setmetatable(table, mod_mt)
end

function cbclua_is_module(table)
	return getmetatable(table) == mod_mt
end

function cbclua_is_codemod(table)
	return cbclua_is_module(table) and table._CODEMOD
end

function cbclua_unload_all_codemods()
	for _, modname in pairs(all_codemod_names) do
		package.loaded[modname] = nil
	end
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
	
	local partial_exports = rawget(mod, "_PARTIAL_EXPORTS")
	if partial_exports then
		for _,partial in ipairs(partial_exports) do
			if partial.names[key] then
				return partial.mod[key]
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
			local ok, result = pcall(require, modname)
			
			if ok then
				mod[key] = result
				return result
			elseif not result:match("module '" .. modname .. "' not found:") then
				error(result, 2)
			end
		end
	end
end
	
local function tryopen(fname)
	local f = io.open(fname)
	if f then
		f:close()
		return true
	end
end
	
local function loader(name)
	local iscodemod = not name:match("^cbclua%.")
	
	local basename
	if iscodemod then
		basename = cbclua_get_codepath() .. name:gsub("%.", "/")
	else
		basename = cbclua_get_modpath() .. name:gsub("^cbclua%.", ""):gsub("%.", "/")
	end
	
	local standardfilename = basename .. ".lua"
	local modfilename = basename .. "/mod.lua"
	
	local filename
	if tryopen(standardfilename) then
		filename = standardfilename
	elseif tryopen(modfilename) then
		filename = modfilename
	else
		return "\n\tno CBCLua module '" .. name .. "'"
	end
	
	if iscodemod then
		table.insert(all_codemod_names, name)
	end
	
	return function ()
		local modfunc, err = loadfile(filename)
		
		if modfunc == nil then
			error("error loading module '" .. name .. "' from file " .. filename .. ":\n" .. err, 0)
		end
		
		local mod = { _NAME = name, _CODEMOD = codemod }
	
		setmetatable(mod, mod_mt)
		setfenv(modfunc, mod)
	
		modfunc()
		
		package.loaded[name] = mod -- this, for some reason, must happen inside the module function
	end
end

table.insert(package.loaders, 1, loader)

