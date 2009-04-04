-- This file is the lua program run by the run script

-- First we perform a few tweaks to our environment

-- Lua's module function makes the module visible as a global variable, which is
-- generally considered a Bad Thing (tm): http://lua-users.org/wiki/LuaModuleFunctionCritiqued
-- Here's a replacement module function that doesn't make the module visible as a global variable
-- When you require something, you must save it to a local variable yourself to actually use the module

local mod_mt = { }

function module(name, ...)
	local mod = { }
	setmetatable(mod, mod_mt)
	mod._NAME = name
	mod._M  = mod
	mod._PACKAGE = name:gsub("[^.]*$", "")
	mod._G = _G -- _G can be used to make true global variables if so desired
	mod._IMPORTS = { }
	package.loaded[name] = mod

	setfenv(2, mod)
	
	for _, f in ipairs({...}) do
		f(mod)
	end
end

-- This function loads a module, then makes it visible in the current module

function import(name)
	local newmod = require(name)
	local outermod = getfenv(2)
	
	outermod._IMPORTS:insert(newmod)
end

-- This function is a metatable function that allows modules to view global functions defined by lua

function mod_mt.__index(mod, key)
	for _,importmod in ipairs(mod) do
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

-- Now we begin the actual startup code

print "CBCLua v0.1 loading"

-- This creates the main task and begins running it

local task = require("std.task")
local mainmod = require("main")

if mainmod == nil then
	error("Main module is missing the main function!")
end

print "Program start"

task.new(mainmod.main)
task.run()
