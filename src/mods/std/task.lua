---- std.task ----
-- This module contains the task scheduler and related functions.
-- It implements co-operative multitasking in lua using coroutines.

module("std.task")

local timer = require "std.timer"
local co = require "coroutine"
local debug = require "debug"
local os = require "os"
local math = require "math"

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
	
	tasklist[id] = { co = co.create(func), sleeptill = 0, name = name or "#" .. id }
	
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
		print("task: program terminated by task " .. curtask.name .. " :")
		print(msg)
	else
		print("task: program terminated by task " .. curtask.name)
	end
	
	os.exit(code or 1)
end

-- Pause current task for a minimum of secs seconds
function sleep(secs)
	tasklist[tasklist_current].sleeptill = timer.seconds() + secs -- set processes sleep amount to the requested amount
	yield()
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
		local curtime = timer.seconds()
		
		for curtask = 1,tasklist_nextid do
			tasklist_current = curtask	
			local task = tasklist[curtask]
			
			if task and task.sleeptill <= curtime then -- if the task is valid and not currently sleeping
				local goodresume = co.resume(task.co)	
							
				if not(goodresume) then -- if the coroutine raised an error
					print("--------")
					print(debug.traceback(task, "error in task " .. curtask .. ": " .. msg, 1)) -- stack trace
					return false
				elseif co.status(task.co) == "dead" then -- if the coroutine ended
					stop(curtask) -- take it off the list
				end
			
				timer.watchdog() -- if this doesn't get called enough the timer module produces a stall warning
				curtime = timer.seconds()
			end
		end
	end
	
	return true
end

function run_sleep() 
	local minsleeptill = math.huge
	
	for checktask = 1,tasklist_nextid do -- go through each process
		local task = tasklist[checktask]
		if task then
			local sleeptill = task.sleeptill
			if sleeptill == 0 then -- if its not sleeping
				minsleeptill = -1 -- set a flag & exit the loop
				break
			elseif sleeptill < minsleeptill then -- if it sleeping but needs to be woken up sooner than our current soonest
				minsleeptill = sleeptill -- then it is now the current soonest
			end
		end
	end
	
	assert(minsleeptill ~= math.huge)
	
	if minsleeptill == -1 then -- if at least one process isn't sleeping
		timer.yield() -- we only yield
	else
		local sleeptime = minsleeptill - timer.seconds() -- if they're all sleeping
		if sleeptime > 0 then
			timer.rawsleep(sleeptime) -- then give the entire process naptime
		end
	end
end

