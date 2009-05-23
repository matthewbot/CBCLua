-- This module holds functions that are used by a task to control
-- its execution

module(...)

local sched = require "std.task.sched"
local timer = require "std.timer"
local coroutine = require "coroutine"
local math = require "math"

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
	return coroutine.yield()
end

function sleep_cycle_till(endtime) -- sleeps for one "task cycle" or until endtime, whichever comes first
	return coroutine.yield{ endtime = endtime }
end

function yield() -- allows other tasks to run but resumes this one ASAP
	return coroutine.yield{ endtime = 0 }
end

-- Wait functions (all use predicates)

-- waits until pred is true, or time to pass
function wait(pred, time, tdelta) 
	if type(pred) == "table" and pred.wait then -- if its a table/object with a wait method
		return pred:wait(time, tdelta) -- call the wait method instead (this lets it be used with signals for instance)
	end
	
	return wait_any({ [true] = pred }, time, tdelta) -- otherwise, this is just wait_any with a single predicate
end

function wait_any(preds, time, tdelta)
	if tdelta == nil then
		tdelta = 0.05
	end
	
	local endtime 
	if time ~= nil then 
		endtime = timer.seconds() + time
	end
	
	while true do
		for name,pred in pairs(preds) do
			if pred() then
				return name
			end
		end
		
		if endtime ~= nil then 
			local remaining = endtime - timer.seconds()
			if remaining < 0 then
				return false
			end
			
			sleep(math.min(remaining, tdelta))
		else
			sleep(tdelta)
		end
	end
end
	
