local sched = require "cbclua.task.sched"
local timer = require "cbclua.timer"
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

function Signal:wait(timeout)
	local curtask = sched.get_current_task()
	table.insert(self.wakelist, curtask)
	curtask:add_task_observer(self)
	
	if timeout then
		curtask:sleep(timeout)
	else
		curtask:suspend()
	end
end

-- TaskObserver Callback

local debug = require "debug"

function Signal:taskobserver_state_changed(task, state, prevstate)
	if state ~= "active" then
		return
	end

	local pos=1
	while pos <= #self.wakelist do
		if self.wakelist[pos] == task then
			break
		end
	end
	
	assert(pos <= #self.wakelist, "Notified from task not on wakelist?")
	table.remove(self.wakelist, pos)
	return false -- don't need any more notifications
end

