-- This module holds functions that are used by a task to control
-- its execution

local sched = require "cbclua.task.sched"
local timer = require "cbclua.timer"
local coroutine = require "coroutine"

-- Primitive sleep functions

function sleep(time) -- sleeps until time has passed
	return sleep_till(time + timer.seconds())
end

function sleep_till(endtime) -- sleeps until endtime is reached, which is defined as number of seconds since the program started
	local yieldargs = { endtime = endtime }
	
	repeat
		coroutine.yield(yieldargs)
	until timer.seconds() >= endtime	
end

function sleep_io(file) -- sleeps until file has data to read
	local yieldargs = { file = file }

	repeat
		coroutine.yield(yieldargs)
	until sched.check_io_flag(file)
end

function sleep_cycle() -- sleeps for one "task cycle", waking up when another process needs to be run
	return coroutine.yield({})
end

function sleep_cycle_till(endtime) -- sleeps for one "task cycle" or until endtime, whichever comes first
	return coroutine.yield{ endtime = endtime }
end

function yield() -- allows other tasks to run but resumes this one ASAP
	return coroutine.yield{ endtime = 0 }
end

