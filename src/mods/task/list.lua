module(...)

local coroutine = require "coroutine"
local os = require "os"

import "cbclua.task.task"

-- Private State

local tasks = { } -- set of all tasks (keys are tasks, value is always true)
local tasks_tostart = { } -- set of all tasks waiting to be started
local task_count = 0
local current_task

-- Public functions

function get_current()
	return current_task
end

function start(...) -- args are func, name, daemon, cstack
	local task = Task(...)
	tasks_tostart[task] = true
	task_count = task_count + 1
	return task
end

function stop(task)
	task:kill()
	if task == current_task then -- special case, stop on the current task
		-- calling kill has already caused the task to "stop", now we just yield to the scheduler
		-- which will see that the task has ended and call stop() again, removing it from the task list
		coroutine.yield()
	end
	
	if tasks_tostart[task] then
		tasks_tostart[task] = nil
		task_count = task_count - 1
	elseif tasks[task] then
		tasks[task] = nil
		task_count = task_count - 1
	end
end

function count()
	return task_count
end

function running_tasks()
	return pairs(tasks)
end

function dump_list()
	for task in running_tasks() do
		local buf = ""
				
		if task:is_system() then
			buf = buf .. "S"
		else
			buf = buf .. "T"
		end
		
		if task == get_current() then
			buf = buf .. "["
		else
			buf = buf .. "|"
		end
			
		buf = buf .. " " .. task:get_name()
		print(buf)
	end
end

function stop_all_user_tasks()
	for task, _ in pairs(tasks) do
		if not task:is_system() and not task == current_task then
			stop(task)
		end
	end
	
	if not current_task:is_system() then
		stop(current_task)
	end
end

function os.exit()
	print("<<< Program exited >>>")
	stop_all_user_tasks()
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

-- for sched

function set_current_task(task)
	current_task = task
end
