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

To determine which tasks to run, the schedular looks at three things,
the tasks' sleeptill field (set by sleep), sleepevent (set by sleep_event),
and sleepsignal (set by Signal:wait()).

If the sleeptill specifies a time in the past, the task is 
considered ready. 

A task with a sleepevent field holding a string event is considered ready
if the sleep function signals the event.

A task with a sleepsignal is considered ready if the sleepsignals's ctr
is higher than the task's signalctr

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
	
	tasklist[id] = { co = co.create(func), id = id, name = name or tostring(id) }
	
	return id
end

-- Causes the current task to yield, giving other tasks a chance to run
-- Not calling this will cause other tasks to stall, so be careful!
function yield()
	local task = tasklist[tasklist_current]
	task.sleeptill = 0
	co.yield()
	task.sleeptill = nil
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
	co.yield()
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
	local task = tasklist[tasklist_current]
	task.sleeptill = timer.seconds() + secs -- set processes sleep amount to the requested amount
	co.yield()
	task.sleeptill = nil
end

-- Waits until the specified event is triggered, or timeout
-- returns true if event was triggered
function sleep_event(event, timeout)
	local curtask = tasklist[tasklist_current]
	
	if timeout then
		curtask.sleeptill = timer.seconds() + timeout -- task system wakes tasks up if the sleeptill or sleepevent passes
	else
		curtask.sleeptill = nil -- nil causes the task system to not wake us until the event
	end
	
	curtask.sleepevent = event
	
	local reason = co.yield()
	
	curtask.sleeptill = nil
	curtask.sleepevent = nil
	
	return reason == "sleepevent" -- return true if reason was the event, false if timeout
end

-- Ends the specified task
function stop(tasknum)
	tasklist[tasknum] = nil
	tasklist_count = tasklist_count - 1
end

-- Signals

Signal = create_class "std.task.Signal"

function Signal:construct()
	self.ctr = 0
end

function Signal:notify()
	self.ctr = self.ctr + 1
end

function Signal:wait(timeout)
	local task = tasklist[tasklist_current]
	task.sleepsignal = self
	task.signalctr = self.ctr
	
	if timeout then
		task.sleeptill = timer.seconds() + timeout
	end
	
	co.yield()
	
	task.sleepsignal = nil
	task.signalctr = nil
	task.sleeptill = nil
end

-- Schedular implementation stuff

-- predeclare some functions
local run_cycle
local run_sleep
local find_min_sleep
local task_is_ready
local task_get_sleep
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
	
	local result = true
	
	while tasklist_count >= 1 do -- while there are processes
		local events = run_sleep() -- sleeps until the nearest task wants to wake up

		if not(run_cycle(events)) then
			result = false
			break
		end
	end
	
	timer.watchdog_term() -- must be sure to term watchdog, it occasionally will segfault the process if the shared object is unloaded while the thread is still running
	
	return result
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
	local hascheck = false
	
	if task.sleepevent ~= nil then -- determine and form the correct ready check
		hascheck = true
		if events[task.sleepevent] then return "sleepevent" end
	end
	
	if task.sleepsignal ~= nil then
		hascheck = true
		if task.sleepsignal.ctr > task.signalctr then return "sleepsignal" end
	end
	
	if task.sleeptill ~= nil then 
		hascheck = true
		if task.sleeptill <= curtime then return "sleeptill" end
	end
	
	if not(hascheck) then
		return "yield" -- so its always ready
	end
	
	-- if the ready check failed
	
	return nil
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
		print(debug.traceback(tco, "error in task '" .. task.name .. "':\n" .. msg)) -- stack trace
		return false
	elseif co.status(tco) == "dead" then -- if the coroutine ended
		stop(id) -- take it off the list
	end
	
	return true
end

function run_sleep() 
	local minsleep = find_min_sleep() -- find the amount of time till the next task needs to wake up
	
	if minsleep == nil then -- if at least one process isn't sleeping (and needs to be re-awaken ASAP)
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
	local minsleeptill = math.huge
	
	for checktask = 1,tasklist_nextid do -- go through each process
		local task = tasklist[checktask]
		if task then
			local sleepamt = task_get_sleep(task)
			if sleepamt == nil then -- if its not sleeping
				return nil -- return sentinel
			elseif sleepamt < minsleeptill then -- if it sleeping but needs to be woken up sooner than our current soonest
				minsleeptill = sleepamt -- then it is now the current soonest
			end
		end
	end

	return minsleeptill
end

-- returns how much longer the task wants to sleep
function task_get_sleep(task)
	if task.sleeptill ~= nil then
		return task.sleeptill
	elseif task.sleepevent ~= nil then
		return math.huge
	elseif task.sleepsignal ~= nil then
		return math.huge
	else
		return nil
	end
end

set_sleep_func(function (time) 
	if time == math.huge then -- we've got no events to wake us up, so if we ever get this the program is over
		print("task: deadlock")
		os.exit(1)
	end

	if time > 0 then -- if we're supposed to go to sleep
		timer.raw_sleep(time) -- then give the entire process naptime
	else -- no sleeping
		timer.raw_yield() -- just yield the process
	end
	
	return { }
end)

