module("cbclua.task")

export "cbclua.task.task"
export "cbclua.task.signal"
export "cbclua.task.util"
export "cbclua.task.control"

local sched = require "cbclua.task.sched"
get_current_task = sched.get_current_task

new = Task -- calling new creates a new Task
function start(...)
	local task = Task(...)
	task:start()
	return task
end
