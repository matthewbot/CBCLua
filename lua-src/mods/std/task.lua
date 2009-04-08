---- std.task ----
-- This module contains the task scheduler and related functions.
-- It implements co-operative multitasking in lua using coroutines.

module("std.task")

local timer = require "std.timer"
local co = require "coroutine"
local debug = require "debug"
local os = require "os"

 -- This is the list of couroutines that are currently running
local tasklist = { }
local tasklist_nextid = 1
local tasklist_count = 0
local tasklist_current

 -- Creates a new task and returns a task number
function start(func, name) 
	if type(func) ~= "function" then
		error("bad argument #1 to 'task.new' (Lua function expected)");
	end

	local id = tasklist_nextid -- some book-keeping
	tasklist_nextid = tasklist_nextid + 1
	tasklist_count = tasklist_count + 1
	
	tasklist[id] = { co = co.create(func), sleepamt = 0, name = name or "#" .. id }
	
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
function get_running_count()
	return tasklist_count
end

-- Ends the current task
function exit()
	stop(tasklist_current)
	yield()
end

-- Ends the entire program, printing the message
function terminate(msg, code)
	if msg ~= nil then
		print("task: program terminated by task " .. curtask.name .. ":")
		print(msg)
	else
		print("task: program terminated by task " .. curtask.name)
	end
	
	os.exit(code or 1)
end

-- Pause current task for a minimum of secs seconds
function sleep(secs)
	local selftask = tasklist[tasklist_current]
	local start = timer.seconds() -- remember starting time
	
	selftask.sleepamt = secs -- set processes sleep amount to the requested amount
	
	while true do 
		local newtime = yield()
		
		local newamt = start + secs - newtime -- recalculate time left to sleep
		if newamt > 0 then -- if there is still some left
			selftask.sleepamt = newamt -- update it, hit the yield again
		else -- times up!
			break -- end the yield loop
		end
	end
	
	selftask.sleepamt = 0
end

-- Ends the specified task
function stop(tasknum)
	tasklist[tasknum] = nil
	tasklist_count = tasklist_count - 1
end

local run_sleep -- predeclare function
		
-- This function runs the tasks in order. It is called from start.lua, and shouldn't be used outside of there
function run()
	while tasklist_count >= 1 do 
		run_sleep() -- sleeps until the nearest task wants to wake up
	
		local time = timer.seconds()
	
		for curtask = 1,tasklist_nextid do
			tasklist_current = curtask	
			local task = tasklist[curtask]
			
			if task then
				local status = co.resume(task.co, time)	
								
				if status == false then -- if the coroutine raised an error
					print("--------")
					print(debug.traceback(task, "error in task " .. curtask .. ": " .. msg, 1)) -- stack trace
					return false
				elseif co.status(task.co) == "dead" then -- if the coroutine ended
					stop(curtask) -- take it off the list
				end
				
				timer.watchdog() -- if this doesn't get called enough the timer module produces a stall warning
			end
		end
	end
	
	return true
end

function run_sleep() 
	local minsleep = 999999
	
	for checktask = 1,tasklist_nextid do
		local task = tasklist[checktask]
		if task then
			local sleepamt = task.sleepamt
			if sleepamt < minsleep then
				minsleep = sleepamt
			end
		end
	end
	
	if minsleep > 0 then
		timer.rawsleep(minsleep)
	end 
end

