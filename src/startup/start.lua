-- This file is the lua program run by the run script

io.stdout:setvbuf("no") -- turn off standard output buffering
io.stdin:setvbuf("no")

-- load the other startup files

local startupdir = arg[0]:match("(.*)\/") .. "/"
local function dostartup(file)
	dofile(startupdir .. file)
end

dostartup("class.lua")
dostartup("module.lua")
dostartup("util.lua")

-- Load system modules

local task = require "std.task"
local sched = require "std.task.sched"
local log = require "std.log" -- This replaces global print with something that logs

-- Then do mode and host specific setup

host = arg[1]
mode = arg[2]

if mode == "interact" then
	dostartup("interact.lua")
else
	if host == "chumby" then
		debug.traceback = cbctraceback
	end
	
	dostartup("runmain.lua")
end

-- Finally, enter the task schedular!

if sched.run() then
	print("Program finished")
else
	print("Program terminated due to errors")
	os.exit(1)
end
