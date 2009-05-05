---- std.task ----
--[[

This module contains the task scheduler and related functions.
It implements co-operative multitasking in lua using coroutines.

Its fairly beastly, and I'd refactor it into something more OO/cleaner/multiple files except I've
probably created and then fixed 15+ bugs in this thing over writing this version, and I'd probably
make many more in rewriting it.

TODO: rewrite description

]]
module("std.task")

local timer = require "std.timer"
local co = require "coroutine"
local debug = require "debug"
local os = require "os"
local math = require "math"
local table = require "table"

import("std.log")

 -- This is the list of couroutines that are currently running
local tasklist = { }
local tasklist_nextid = 1
local tasklist_count = 0
local tasklist_current

 -- Creates a new task and returns a task number
function start(func, name, daemon, cstack) 
	if type(func) ~= "function" then
		error("bad argument #1 to 'task.start' (Lua function expected)")
	end
	
	local id = tasklist_nextid -- some book-keeping
	tasklist_nextid = tasklist_nextid + 1
	tasklist_count = tasklist_count + 1
	
	name = name or tostring(id)
	
	tasklist[id] = { co = co.create(func, cstack and 0 or -1), id = id, name = name, daemon = daemon }
	
	log("started task " .. name)
	
	return id
end

-- Causes the current task to yield, giving other tasks a chance to run
-- Not calling this will cause other tasks to stall, so be careful!
function yield()
	local curtask = tasklist[tasklist_current]
	curtask.sleeptill = 0
	co.yield()
end

-- Returns the task number of the current task
function get_current()
	return tasklist_current
end

function get_name(task)
	local task = tasklist[task]
	if task then
		return task.name
	else
		return nil
	end
end

-- Returns the number of tasks currently running
function get_running_count()
	return tasklist_count
end

-- Ends the current task
function exit()
	local task = task[tasklist_current]
	stop(tasklist_current, true)
	log("task " .. task.name .. " exited")
	co.yield()
end

-- Ends the entire program, printing the message
function terminate(msg, code)
	local curtask = tasklist[tasklist_current]

	if msg ~= nil then
		log("program terminated by task " .. curtask.name .. " :\n" .. msg)
	else
		log("program terminated by task " .. curtask.name)
	end
	
	os.exit(code or 0)
end

-- Pause current task for a minimum of secs seconds
function sleep(secs)
	local task = tasklist[tasklist_current]
	task.sleeptill = timer.seconds() + secs -- set processes sleep amount to the requested amount
	co.yield()
end

-- Waits until data comes in on the specified file, or timeout passes
function sleep_io(file, timeout)
	local curtask = tasklist[tasklist_current]
	
	if timeout then
		curtask.sleeptill = timer.seconds() + timeout -- task system wakes tasks up if the sleeptill or sleepevent passes
	end
	
	curtask.sleepfile = file
	
	return co.yield() == "sleepfile" -- return true if reason was the event, false if timeout
end

-- Ends the specified task
function stop(tasknum, nolog)
	local task = tasklist[tasknum]
	if task == nil then 
		if not nolog then
			error("stopping bad task id " .. tasknum, 2) 
		else
			return
		end
	end
		
	tasklist[tasknum] = nil
	tasklist_count = tasklist_count - 1
	
	if nolog ~= true then
		log("stopped task " .. task.name)
	end
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
	
	return co.yield() == "sleepsignal"
end

-- Schedular implementation stuff

-- predeclare some functions
local run_cycle
local run_sleep
local find_min_sleep
local find_all_files
local task_is_ready
local task_get_sleep
local resume_task
local all_tasks_ended
		
-- This function runs the tasks in order. It is called from start.lua, and shouldn't be used outside of there
function run()
	collectgarbage("stop") -- stop GC
	collectgarbage("collect") -- do a full collection to clean up any leftovers from loading the program
	
	local result = true
	
	while not all_tasks_ended() do -- while there are processes
		local files = run_sleep() -- sleeps until the nearest task wants to wake up

		if not(run_cycle(files)) then
			result = false
			break
		end
	end

	return result
end

function run_cycle(files)
	local curtime = timer.seconds()
	
	for taskid=1,tasklist_nextid do -- for each task id
		local task = tasklist[taskid]
		if task ~= nil then
			local reason = task_is_ready(task, curtime, files)
			if reason then -- if the task is ready		
				timer.watchdog() -- if this doesn't get called enough the timer module produces a stall warning
			
				if not(resume_task(task, reason)) then return false end
				curtime = timer.seconds()
			end
		end
	end
	
	collectgarbage("step")
	
	return true
end

function task_is_ready(task, curtime, files)
	local hascheck = false
	
	if task.sleepfile ~= nil then -- determine and form the correct ready check
		hascheck = true
		if files[task.sleepfile] then return "sleepfile" end
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
	
	task.sleepsignal = nil -- clear its sleep fields
	task.sleepfile = nil
	task.sleeptill = nil
	
	tasklist_current = id 
	local goodresume,msg = co.resume(tco, reason) -- perform resume
	tasklist_current = nil
				
	if not(goodresume) then -- if the coroutine raised an error
		log(true, debug.traceback(tco, "error in task '" .. task.name .. "':\n" .. msg)) -- stack trace
		return false
	elseif co.status(tco) == "dead" then -- if the coroutine ended
		stop(id, true) -- take it off the list
		log("task " .. task.name .. " ended")
	end
	
	return true
end

function run_sleep() 
	local minsleep = find_min_sleep() -- find the amount of time till the next task needs to wake up
	local files = find_all_files()
	
	local sleeptime
	
	if minsleep == math.huge then -- if no process needs to be woken up
		sleeptime = -1 -- pass -1 to the C function
	else
		sleeptime = minsleep - timer.seconds()  -- calculate the time we need to sleep
		
		if sleeptime < 0 then
			sleeptime = 0
		end
	end
	
	if sleeptime == -1 and #files == 0 then
		log("deadlock, ending program :(")
		os.exit(1)
	end

	local bools = { timer.raw_sleep(sleeptime, unpack(files)) }
	local gotfiles = { }
	
	for num,file in ipairs(files) do
		if bools[num] then
			gotfiles[file] = true
		end
	end
	
	return gotfiles
end

function find_min_sleep()
	local minsleeptill = math.huge
	
	for checktask = 1,tasklist_nextid do -- go through each process
		local task = tasklist[checktask]
		if task then
			local sleepamt = task_get_sleep(task)
			
			if sleepamt == 0 then
				return 0
			end
			
			if sleepamt < minsleeptill then -- if it sleeping but needs to be woken up sooner than our current soonest
				minsleeptill = sleepamt -- then it is now the current soonest
			end
		end
	end

	return minsleeptill
end

function find_all_files()
	local files = { }
	local files_set = { }
	
	for checktask = 1,tasklist_nextid do
		local task = tasklist[checktask]
		if task then
			local file = task.sleepfile
			if file and files_set[file] == nil then
				table.insert(files, file)
				files_set[file] = true
			end
		end
	end
	
	return files
end

-- returns how much longer the task wants to sleep
function task_get_sleep(task)
	if task.sleeptill ~= nil then
		if task.sleepsignal ~= nil and task.signalctr < task.sleepsignal.ctr then
			return 0
		else
			return task.sleeptill
		end
	elseif task.sleepfile ~= nil then
		return math.huge
	elseif task.sleepsignal ~= nil then
		return math.huge
	else
		return 0
	end
end

-- returns true if all non-daemon tasks have ended
function all_tasks_ended()
	if tasklist_count == 0 then
		return true
	end
	
	for i = 1,tasklist_nextid do
		local task = tasklist[i]
		if task and not task.daemon then
			return false
		end
	end
	
	return true
end

