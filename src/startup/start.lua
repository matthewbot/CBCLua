-- This file is the lua program run by the run script

io.stdout:setvbuf("no") -- turn off standard output buffering
io.stdin:setvbuf("no")

-- get host from arg list
host = arg[1]

-- load the other startup files

local startupdir = arg[0]:match("(.*)\/") .. "/"
local function dostartup(file)
	dofile(startupdir .. file)
end

dostartup("config.lua")
dostartup("class.lua")
dostartup("module.lua")

--[[
-- Start system tasks
local list = require "cbclua.task.list"
local main = require "main"

list.start(main.main, "main", false)
]]

-- Start interaction tasks
local interact = require "cbclua.interact"
interact.start_tasks()

-- Finally, enter the task schedular!
local sched = require "cbclua.task.sched"
local ok, msg = sched.run()

if ok then
	print("CBCLua exiting")
else
	print(msg)
	print("CBCLua terminated due to errors")
	os.exit(1)
end

