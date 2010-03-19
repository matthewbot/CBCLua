module("cbclua.task")

export "cbclua.task.entry"
export "cbclua.task.signal"
export "cbclua.task.util"
export "cbclua.task.control"

local sched = require "cbclua.task.sched"
get_current_task = sched.get_current_task

new = TaskEntry -- calling new creates a new Task
function start(...)
	local task = TaskEntry(...)
	task:start()
	return task
end
