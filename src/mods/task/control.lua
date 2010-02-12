local sched = require "cbclua.task.sched"

function sleep(amt)
	return sched.get_current_task():sleep(amt)
end

function sleep_io(io)
	return sched.get_current_task():sleep_io(io)
end

function yield()
	return sched.get_current_task():yield()
end

function stop()
	return sched.get_current_task():stop()
end

