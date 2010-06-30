-- This file is the lua program run by the run script

io.stdout:setvbuf("no") -- turn off standard output buffering
io.stdin:setvbuf("no")

-- load the other startup files

local startupdir = arg[0]:match("(.*)\/") .. "/"
local function dostartup(file)
	dofile(startupdir .. file)
end

dostartup("stdexts.lua")
dostartup("config.lua")
dostartup("class.lua")
dostartup("module.lua")

-- Start console task
if cbclua_get_host() == "chumby" then
	local console = require "cbclua.console"
	console.start_task()
end

local interact = require "cbclua.interact"
if not interact.start_tasks() then
	print("Failed to start Interaction")
end

-- Print welcome message
print("This is '" .. cbclua_get_name() .. "', running " .. cbclua_get_version())

-- Finally, enter the task scheduler!
local schedmain = require "cbclua.sched.main"
return schedmain.sched_main() -- tail call

