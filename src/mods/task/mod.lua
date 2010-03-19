module("cbclua.task")

local sched = require "cbclua.sched"

export "cbclua.task.signal"
export "cbclua.task.util"
export "cbclua.task.control"

new = sched.TaskEntry -- calling new creates a new TaskEntry
function start(...)
	local task = sched.TaskEntry(...)
	task:start()
	return task
end
