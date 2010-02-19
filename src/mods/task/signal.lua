local sched = require "cbclua.task.sched"
local table = require "table"

Signal = create_class "Signal"

function Signal:construct()
	self.wakelist = { }
end

function Signal:notify()
	while #self.wakelist > 0 do
		local waketask = table.remove(self.wakelist, 1)
		if waketask:get_state() == "suspended" then
			waketask:remove_task_observer(self)
			waketask:resume()
			break
		end
	end
end

function Signal:notify_all()
	for _, task in ipairs(self.wakelist) do
		if task:get_state() == "suspended" then
			task:remove_task_observer(self)
			task:resume()
		end
	end
	
	self.wakelist = { }
end

function Signal:wait()
	local curtask = sched.get_current_task()
	table.insert(self.wakelist, curtask)
	curtask:add_task_observer(self)
	curtask:suspend()
end

-- TaskObserver Callback

function Signal:tasksobserver_state_changed(task, state, prevstate)
	assert(prevstate == "suspended", "Notified from task not suspended?")
	
	local pos=1
	while pos <= #self.waitlist do
		if self.waitlist[pos] == task then
			break
		end
	end
	
	assert(pos <= #self.waitlist, "Notified from task not on waitlist?")
	table.remove(self.waitlist, pos)
	return false -- don't need any more notifications
end

