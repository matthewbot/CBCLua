local task = require "cbclua.task"
local tasks = require "cbclua.interact.tasks"
local userprgm = require "cbclua.userprgm"
require "cbclua.interact.hooks" -- set up interaction hooks

function start_tasks()
	userprgm.load_interaction()

	task.start(tasks.listen, "interact listener", "system")
	task.start(tasks.respond, "interact respond", "system")
	task.start(tasks.update_task_lists, "interact TLU", "system")
end

