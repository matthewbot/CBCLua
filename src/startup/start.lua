-- This file is the lua program run by the run script

io.stdout:setvbuf("no") -- turn off standard output buffering

-- load the other startup files

startupdir = arg[0]:match("(.*)\/")
dofile(startupdir .. "/module.lua")
dofile(startupdir .. "/util.lua")

-- Scan the argument list to see if lua was launched in interactive mode

if hasarg("-i") then
	print "cbclua: beginning interaction"
	
	package.preload['std.task'] = function () error("module std.task not supported in interact mode") end
	
	return
end

-- Scan to see if we were launched in cbc mode

if hasarg("cbc") then
	debug.traceback = cbctraceback -- patch debug.traceback to not use tabs so they're readable on CBC display
end

-- Load the main module

local status, result = xpcall(function() 
	return require("main") 
end, function (errmsg)
	print(debug.traceback("cbclua: error while loading main module: " .. errmsg, 2))
end)

if status == false then
	os.exit(1)
end

-- Make sure the main module looks ok

local mainmod = result

if type(mainmod) ~= "table" then
	print("cbclua: main module is missing the module header")
	os.exit(1)
end
	
local mainfunc = mainmod.main
	
if mainfunc == nil then
	print("cbclua: main module is missing main function")
	os.exit(1)
end

local task = require "std.task"
task.start(mainfunc)

-- Print a final status message
print("cbclua: starting main task")
dostartup() -- if the program has a startup function run it
if task.run() then
	print("cbclua: program finished")
else
	print("cbclua: program terminated due to errors")
	os.exit(1)
end
