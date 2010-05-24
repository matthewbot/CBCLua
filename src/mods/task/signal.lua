module("cbclua.task.signal")

local sched = require "cbclua.sched"
local timer = require "cbclua.timer"
local table = require "table"
import "cbclua.task.control"

Signal = create_class "Signal"

function Signal:construct()
	self.wakelist = { }
end

function Signal:notify()
	local waketask = table.remove(self.wakelist, 1)
	if not waketask then
		return false
	end
	
	sched.add_task(waketask)
	return true
end

function Signal:notify_all()
	if #self.wakelist == 0 then
		return false
	end

	for task in table.values(self.wakelist) do
		sched.add_task(task)
	end
	self.wakelist = { }
	return true
end

function Signal:wait(timeout)
	local curtask = sched.get_current()
	sched.remove_task(curtask, "Signal")
	table.insert(self.wakelist, curtask)
	
	local timerentry
	if timeout then
		timerentry = sched.TimerEntry(timer.seconds() + timeout, function ()
			sched.add_task(curtask, "timeout")
		end)
		sched.register_timer(timerentry)
	end
	
	local resumearg = yield_with_cleanup(function ()
		if timerentry then
			sched.unregister_timer(timerentry)
		end
		
		local pos = table.findvalue(self.wakelist, curtask)
		table.remove(self.wakelist, pos)		
	end)
	
	return resumearg ~= "timeout"
end

