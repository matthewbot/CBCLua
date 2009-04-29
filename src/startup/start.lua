-- This file is the lua program run by the run script

io.stdout:setvbuf("no") -- turn off standard output buffering
io.stdin:setvbuf("no")

-- load the other startup files

startupdir = arg[0]:match("(.*)\/")
dofile(startupdir .. "/class.lua")
dofile(startupdir .. "/module.lua")
dofile(startupdir .. "/util.lua")

-- Scan to see if we were launched in cbc mode

if hasarg("cbcconsole") then
	debug.traceback = cbctraceback -- patch debug.traceback to not use tabs so they're readable on CBC display
end

-- Then do mode-specific setup

if hasarg("interact") then
	dofile(startupdir .. "/interact.lua")
else
	dofile(startupdir .. "/runmain.lua")
end

-- Finally, enter the task schedular!

local task = require "std.task"
if task.run() then
	print("cbclua: program finished")
else
	print("cbclua: program terminated due to errors")
	os.exit(1)
end
