-- This file is the lua program run by the run script

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
	cbcluamodule(mod)
	mod._AUTOGLOBALS = true
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
	
function kissc_compat() 
	local kissc = require "std.kissc"
	kissc.compat(2)
end

-- Scan the argument list to see if lua was launched in interactive mode

local interact = false

for _,v in pairs(arg) do
	if v == "-i" then
		interact = true
		break
	end
end

if interact then
	print "cbclua: v0.1 interact"
	
	package.preload['std.task'] = function () error("module std.task not supported in interact mode") end
	
	return
end

-- Now we begin the actual startup code

local goterror = false

local status, trace = xpcall(function() 
	print "cbclua: v0.1 loading"

	-- This creates the main task and begins running it

	local mainmod = require("main")

	if mainmod == nil then
		error("Main module is missing the main function!")
	end
	
	print "cbclua: main task starting"
	local task = require("std.task")
	task.start(mainmod.main)
	if task.run() == false then -- task.run signals an error by returning false
		goterror = true
	end
end, function (errmsg)
	-- error handler
	return debug.traceback("error at top level: " .. errmsg, 2)
end)

-- The task scheduler catches errors inside tasks
-- If an error still gets through to the outer xpcall() then we need to still give a stack trace
if status == false then
	print("--------")
	print(trace)
	goterror = true
end

-- Print a final status message
if not(goterror) then
	print("cbclua: finished")
else
	print("cbclua: terminated due to errors")
end
