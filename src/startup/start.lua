-- This file is the lua program run by the run script

io.stdout:setvbuf("no") -- turn off standard output buffering
io.stdin:setvbuf("no")

-- set mode and host from arg list
host = arg[1]
mode = arg[2]

-- load the other startup files

local startupdir = arg[0]:match("(.*)\/") .. "/"
local function dostartup(file)
	dofile(startupdir .. file)
end

dostartup("class.lua")
dostartup("module.lua")

-- Start system tasks
--TODO

-- Finally, enter the task schedular!
local sched = require "std.task.sched"
local ok, msg = sched.run()

if ok then
	print("CBCLua exiting")
else
	print(msg)
	print("CBCLua terminated due to errors")
	os.exit(1)
end

