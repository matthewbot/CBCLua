---- std.task ----
--[[

This module contains the task scheduler and related functions.
It implements co-operative multitasking in lua using coroutines.

TODO: rewrite description

]]
module("std.task")

local timer = require "std.timer"
local co = require "coroutine"
local debug = require "debug"
local os = require "os"
local math = require "math"
local table = require "table"

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
	
	tasklist[id] = { co = co.create(func), id = id, name = name or tostring(id), sleeptill = 0 }
	
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
		
-- This function runs the tasks in order. It is called from start.lua, and shouldn't be used outside of there
function run()
	collectgarbage("stop") -- stop GC
	collectgarbage("collect") -- do a full collection to clean up any leftovers from loading the program
	
	local result = true
	
	while tasklist_count >= 1 do -- while there are processes
		local files = run_sleep() -- sleeps until the nearest task wants to wake up

		if not(run_cycle(files)) then
			result = false
			break
		end
	end
	
	timer.watchdog_term() -- must be sure to term watchdog, it occasionally will segfault the process if the shared object is unloaded while the thread is still running
	
	return result
end

function run_cycle(files)
	local curtime = timer.seconds()
	
	for taskid, task in ipairs(tasklist) do -- for each task id
		local reason = task_is_ready(task, curtime, files)
		if reason then -- if the task is ready		
			timer.watchdog() -- if this doesn't get called enough the timer module produces a stall warning
			
			if not(resume_task(task, reason)) then return false end
			curtime = timer.seconds()
		end
	end
	
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
		print("task: deadlock")
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
			if sleepamt ~= nil and sleepamt < minsleeptill then -- if it sleeping but needs to be woken up sooner than our current soonest
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
		return task.sleeptill
	elseif task.sleepevent ~= nil then
		return math.huge
	elseif task.sleepsignal ~= nil then
		return math.huge
	else
		return nil
	end
end

