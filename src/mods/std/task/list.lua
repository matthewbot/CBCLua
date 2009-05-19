module(...)

local coroutine = require "coroutine"

import "std.task.task"

-- Private State

local tasks = { } -- set of all tasks (keys are tasks, value is always true)
local tasks_tostart = { } -- set of all tasks waiting to be started

-- Public functions

function start(...) -- args are func, name, daemon, cstack
	local task = Task(...)
	tasks_tostart[task] = true
	return task
end

function stop(task)
	if tasks_tostart[task] then
		tasks_tostart[task] = nil
		return
	end

	if tasks[task] == nil then
		error("Attempting to stop non-existent task " .. task, 2)
	end
	
	tasks[task] = nil
end

function exit()
	tasks[get_current()] = nil
	coroutine.yield()
end

function count()
	return #tasks
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
