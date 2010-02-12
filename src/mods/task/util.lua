local taskmod = require "cbclua.task.task"
local timer = require "cbclua.timer"
local math = require "math"
import "cbclua.task.control"

function async(func, ...)
	local args = {...}
	local function asyncfunc()
		return func(unpack(args))
	end
	
	local task = taskmod.Task(asyncfunc, "async task", "daemon")
	task:start()
	return task
end

-- Wait functions (all use predicates)

-- waits until pred is true, or time to pass
function wait(pred, timeout, tdelta) 
	return wait_any{ [true] = pred, timeout = timeout, tdelta = tdelta } -- otherwise, this is just wait_any with a single predicate
end

-- waits until pred is false, or time to pass
function wait_while(pred, timeout, tdelta)
	return wait(function () return not pred() end, timeout, tdelta)
end

-- waits for pred to become true then false
function wait_toggle(pred, timeout, tdelta)
	wait(pred, timeout, tdelta)
	wait_while(pred, timeout, tdelta)
end

function wait_any(preds)
	local tdelta=0.05
	if preds.tdelta then
		tdelta = preds.tdelta
		preds.tdelta = nil
	end
	
	local endtime 
	if preds.timeout then 
		endtime = timer.seconds() + preds.timeout
		preds.timeout = nil
	end
	
	while true do
		for name,pred in pairs(preds) do
			if pred() then
				return name, pred
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

-- Run func for a maximum amount of time

function timeout(timeout, func)
	local func_ended = false
	local func_results
	local taskid = async(function ()
		func_results = { func() }
		func_ended = true
	end)
	
	wait(function () return func_ended end, timeout)
	
	if not func_ended then
		list.stop(taskid)
		return false
	else
		return true, unpack(func_results)
	end
end

function stop_all_user_tasks()
	for task in taskmod.user_tasks() do
		task:stop()
	end
end

