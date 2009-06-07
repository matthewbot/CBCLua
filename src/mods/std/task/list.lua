module(...)

local coroutine = require "coroutine"

import "std.task.task"

-- Private State

local tasks = { } -- set of all tasks (keys are tasks, value is always true)
local tasks_tostart = { } -- set of all tasks waiting to be started
local task_count = 0

-- Public functions

function start(...) -- args are func, name, daemon, cstack
	local task = pass_errors(Task, ...)
	tasks_tostart[task] = true
	task_count = task_count + 1
	return task
end

function async(func)
	return start(func, "async task", true)
end

function stop(task)
	task:kill()
	
	if tasks_tostart[task] then
		tasks_tostart[task] = nil
	elseif tasks[task] then
		tasks[task] = nil
		task_count = task_count - 1
	end
end

function exit()
	stop(get_current())
	coroutine.yield()
end

function count()
	return task_count
end

function real_count()
	local ctr=0
	
	for task in running_tasks() do
		if not task:is_daemon() then
			ctr = ctr + 1
		end
	end
	
	return ctr
end

function running_tasks()
	return pairs(tasks)
end

function start_new_tasks()
	local started = false

	for task,_ in pairs(tasks_tostart) do
		started = true
		tasks[task] = true
	end
	
	if not started then
		return false
	end
	
	tasks_tostart = { }
	return true
end
