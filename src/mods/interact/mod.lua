module("cbclua.interact")

local task = require "cbclua.task"
local tasks = require "cbclua.interact.tasks"
local userprgm = require "cbclua.userprgm"
require "cbclua.interact.hooks" -- set up interaction hooks

function start_tasks()
	if not tasks.init() then
		return false
	end
	
	userprgm.load_interaction()
	
	task.start(tasks.listen, "interact listener", "system")
	task.start(tasks.respond, "interact respond", "system")
	task.start(tasks.update_task_lists, "interact TLU", "system")
	
	return true
end

