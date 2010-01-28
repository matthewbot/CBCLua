-- This file is the lua program run by the run script

io.stdout:setvbuf("no") -- turn off standard output buffering
io.stdin:setvbuf("no")

-- load the other startup files

local startupdir = arg[0]:match("(.*)\/") .. "/"
local function dostartup(file)
	dofile(startupdir .. file)
end

dostartup("config.lua")
dostartup("class.lua")
dostartup("module.lua")

-- Start console task
local console = require "cbclua.console"
console.start_task()

if arg[2] ~= "nointeract" then
	-- Start interaction tasks
	local interact = require "cbclua.interact"
	interact.start_tasks()
else
	print("Interaction disabled")
end

-- Print welcome message
print("This is '" .. cbclua_get_name() .. "', running " .. cbclua_get_version())

-- Finally, enter the task scheduler!
local sched = require "cbclua.task.sched"
return sched.run() -- tail call

