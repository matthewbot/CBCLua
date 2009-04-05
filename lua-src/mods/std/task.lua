---- std.task ----
-- This module contains the task scheduler and related functions.
-- It implements co-operative multitasking in lua using coroutines.

module("std.task")

local timer = require "std.timer"
local co = require "coroutine"
local debug = require "debug"

 -- This is the list of couroutines that are currently running
local tasklist = { }
local tasklist_nextid = 1
local tasklist_count = 0
local tasklist_current

 -- Creates a new task and returns a task number
function new(func) 
	if type(func) ~= "function" then
		error("bad argument #1 to 'task.new' (Lua function expected)");
	end

	local id = tasklist_nextid -- some book-keeping
	tasklist_nextid = tasklist_nextid + 1
	tasklist_count = tasklist_count + 1
	
	tasklist[id] = co.create(func)
	
	return id
end

-- Causes the current task to yield, giving other tasks a chance to run
-- Not calling this will cause other tasks to stall, so be careful!
yield = co.yield

-- Returns the task number of the current task
function get_current()
	return tasklist_current
end

-- Returns the number of tasks currently running
function get_run_count()
	return tasklist_count
end

-- Ends the current task
function exit()
	endtask(tasklist_current)
end

-- Ends the entire program, printing the message
function terminate(msg)
	co.yield("terminate", msg)
end

-- Pause current task for a minimum of secs seconds
function sleep(secs)
	local start = timer.seconds()
	while timer.seconds() - start < secs do yield() end
end

-- Ends the specified task
function endtask(tasknum)
	tasklist[tasknum] = nil
	tasklist_count = tasklist_count - 1
end
		
-- This function runs the tasks in order. It is called from start.lua, and shouldn't be used outside of there
function run()
	while tasklist_count >= 1 do 
		for curtask = 1,tasklist_nextid do
			timer.watchdog() -- the timer module will print a message if this doesn't get called often enough warning of a program stall
		
			tasklist_current = curtask	
			local task = tasklist[curtask]
			
			if task then
				local noerr, msg, arg = co.resume(task, arg)					
				if noerr == false then
					print("--------")
					print(debug.traceback(task, "error in task " .. curtask .. ": " .. msg, 1))
					return false
				elseif co.status(task) == "dead" then
					endtask(curtask)
				elseif msg == "terminate" then
					if arg ~= nil then
						print("task: program terminated by task " .. curtask .. ":")
						print(arg)
					else
						print("task: program terminated by task " .. curtask)
					end
					
					return false
				end
			end
		end
	end
	
	return true
end

