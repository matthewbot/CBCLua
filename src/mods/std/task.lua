---- std.task ----
--[[

This module contains the task scheduler and related functions.
It implements co-operative multitasking in lua using coroutines.

-- Implementation details / notes to Matt's forgetfullness --

The sleep function is a replaceable part of the task system that
is called whenever the task system determines that every process
is sleeping or waiting for an event. The sleep function causes 
the lua process to sleep for a maximum of the time specified. When
finished, it can optionally return a set of events that may have
caused the sleep to terminate. 

The motivation of the sleep function system is to allow cbclua
programs to boil down to a select loop on the create's serial port
and, perhaps one day, data from cbcui. This allows minimum CPU usage
while still also giving the best possible response time.

To determine which tasks to run, the schedular looks at two things,
the tasks' sleeptill field (set by sleep), and sleepevent (set by sleep_event).
If the sleeptill specifies a time in the past, but not -1, the task is 
considered ready. A task with a sleepevent field holding a string event is
automatically considered ready regardless of sleeptill if the sleep function
signals the event.

]]
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
	
	tasklist[id] = { co = co.create(func), id = id, sleeptill = 0, name = name or "#" .. id }
	
	return id
end

-- Causes the current task to yield, giving other tasks a chance to run
-- Not calling this will cause other tasks to stall, so be careful!
function yield()
	co.yield()
end

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
	local curtask = tasklist[tasklist_current]

	if msg ~= nil then
		print("task: program terminated by task " .. curtask.name .. " :")
		print(msg)
	else
		print("task: program terminated by task " .. curtask.name)
	end
	
	os.exit(code or 0)
end

-- Pause current task for a minimum of secs seconds
function sleep(secs)
	tasklist[tasklist_current].sleeptill = timer.seconds() + secs -- set processes sleep amount to the requested amount
	co.yield()
end

-- Waits until the specified event is triggered, or timeout
-- returns true if event was triggered
function sleep_event(event, timeout)
	local curtask = tasklist[task_current]
	
	if timeout then
		curtask.sleeptill = timer.seconds() + timeout -- task system wakes tasks up if the sleeptill or sleepevent passes
	else
		curtask.sleeptill = -1 -- -1 causes the task system to not wake us until the event
	end
	
	curtask.sleepevent = event
	
	local reason = co.yield()
	return reason == "sleepevent" -- return true if reason was the event, false if timeout
end

-- Ends the specified task
function stop(tasknum)
	tasklist[tasknum] = nil
	tasklist_count = tasklist_count - 1
end

-- predeclare some functions
local run_cycle
local run_sleep
local find_min_sleep
local task_is_ready
local resume_task
local def_sleep_func

-- the sleep function is responsible for sleeping up to the specified amount of time,
-- and returning any "events" that have occured in the mean time
local sleepfunc

function set_sleep_func(newfunc)
	sleepfunc = newfunc
end
		
-- This function runs the tasks in order. It is called from start.lua, and shouldn't be used outside of there
function run()
	collectgarbage("stop") -- stop GC
	collectgarbage("collect") -- do a full collection to clean up any leftovers from loading the program
	
	while tasklist_count >= 1 do -- while there are processes
		local events = run_sleep() -- sleeps until the nearest task wants to wake up

		if not(run_cycle(events)) then return false end
	end
	
	return true
end

function run_cycle(events)
	local curtime = timer.seconds()
	
	for taskid, task in ipairs(tasklist) do -- for each task id
		local reason = task_is_ready(task, curtime, events)
		if reason then -- if the task is ready		
			timer.watchdog() -- if this doesn't get called enough the timer module produces a stall warning
			
			if not(resume_task(task, reason)) then return false end
			curtime = timer.seconds()
		end
	end
	
	return true
end

function task_is_ready(task, curtime, events)
	if task.sleeptill ~= -1 and task.sleeptill <= curtime then
		return "sleeptill"
	elseif task.sleepevent == nil or events[task.sleepevent] then
		return "sleepevent"
	else
		return nil
	end
end

function resume_task(task, reason)
	local id = task.id
	local tco = task.co
	
	task.sleepevent = nil -- clear its sleep fields
	task.sleeptill = 0
	
	tasklist_current = id 
	local goodresume,msg = co.resume(tco, reason) -- perform resume
	tasklist_current = nil
				
	if not(goodresume) then -- if the coroutine raised an error
		print("--------")
		print(debug.traceback(tco, "error in task " .. task.name .. ": " .. msg)) -- stack trace
		return false
	elseif co.status(tco) == "dead" then -- if the coroutine ended
		stop(id) -- take it off the list
	end
	
	return true
end

function run_sleep() 
	local minsleep = find_min_sleep() -- find the amount of time till the next task needs to wake up
	
	if minsleep == -1 then -- if at least one process isn't sleeping (and needs to be re-awaken ASAP)
		collectgarbage("step", 1) -- run a GC step
		return sleepfunc(0) -- call the sleep function to pick up on possible events
	else -- if they're all sleeping
		local sleeptime = minsleep - timer.seconds()  -- calculate the time we need to sleep
		if sleeptime > 0.1 then -- if we're sleeping for more than a tenth of a second
			collectgarbage("collect") -- run a full GC cycle
			sleeptime = minsleep - timer.seconds() -- recalculate the time
		end
		
		if sleeptime < 0 then
			sleeptime = 0
		end
		
		return sleepfunc(sleeptime) -- call sleep function to put us to sleep
	end
end

function find_min_sleep()
	local minsleeptill = 9999
	
	for checktask = 1,tasklist_nextid do -- go through each process
		local task = tasklist[checktask]
		if task then
			local sleeptill = task.sleeptill
			if sleeptill == 0 and task.sleepevent == nil then -- if its not sleeping
				return -1 -- return sentinel
			elseif sleeptill < minsleeptill then -- if it sleeping but needs to be woken up sooner than our current soonest
				minsleeptill = sleeptill -- then it is now the current soonest
			end
		end
	end

	return minsleeptill
end

set_sleep_func(function (time) 
	if time > 0 then -- if we're supposed to go to sleep
		timer.watchdog_disable() -- disable watchdog
		timer.rawsleep(time) -- then give the entire process naptime
	else -- no sleeping
		timer.yield() -- just yield
	end
	
	return { }
end)
