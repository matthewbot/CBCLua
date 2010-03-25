module("cbclua.task.control")

local sched = require "cbclua.sched"
local timer = require "cbclua.timer"
local coroutine = require "coroutine"

--[[ Task creation functions ]]--

function start(...)
	local task = sched.TaskEntry(...)
	sched.add_task(task)
	return task
end

function stop(task)
	local task = task or sched.get_current()
	
	sched.remove_task(task)
	task:stop()
	
	if task == sched.get_current() then
		yield()
	end
end

--[[ Task blocking functions ]]--

function sleep(amt)
	return sleep_till(timer.seconds() + amt)
end

function sleep_till(time)
	local curtask = sched.get_current()
	sched.remove_task(curtask, "sleep")

	local timerentry = sched.new_timer(time, function ()
		sched.add_task(curtask)
	end)
	
	yield_with_cleanup(function ()
		sched.unregister_timer(timerentry)
	end)
end

function sleep_io(file, timeout)
	local curtask = sched.get_current()
	sched.remove_task(curtask, "sleep_io")
	
	local timerentry
	if timeout then
		timerentry = sched.new_timer(timer.seconds() + timeout, function ()
			sched.add_task(curtask, "timeout")
		end)
	end		

	local ioentry = sched.new_ioentry(file, function ()
		sched.add_task(curtask)
	end)
	
	local yieldarg = yield_with_cleanup(function ()
		sched.unregister_ioentry(ioentry)
		if timerentry then
			sched.unregister_timer(timerentry)
		end
	end)
	
	return yieldarg ~= "timeout"
end

function join(othertask, timeout)
	if othertask:get_state() == "stopped" then return true end
	local curtask = sched.get_current()
	assert(curtask ~= othertask, "Can't join the current task!")
	sched.remove_task(curtask, "join")

	local othercleanup = othertask:register_cleanup(function ()
		sched.add_task(curtask)
	end)
	
	local timerentry
	if timeout then
		timerentry = sched.new_timer(timer.seconds() + timeout, function ()
			sched.add_task(curtask, "timeout")
		end)
	end
	
	local yieldarg = yield_with_cleanup(function ()
		othertask:unregister_cleanup(othercleanup)
		if timerentry then
			sched.unregister_timer(timerentry)
		end
	end)
	
	return yieldarg ~= "timeout"
end

--[[ Yields and helpers ]]--

function yield()
	return coroutine.yield()
end

function yield_with_cleanup(cleanup)
	local curtask = sched.get_current()
	
	local id = curtask:register_cleanup(cleanup)
	local yieldarg = coroutine.yield()
	curtask:unregister_cleanup(id)
	
	cleanup()
	return yieldarg
end

