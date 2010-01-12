module(...)

local task = require "cbclua.task"
local tasks = require "cbclua.interact.tasks"

function start_tasks()
	task.start(tasks.listen, "interact listener")
	task.start(tasks.respond, "interact respond", true)
end

