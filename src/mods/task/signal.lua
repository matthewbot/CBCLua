local sched = require "cbclua.task.sched"
local timer = require "cbclua.timer"
local table = require "table"

Signal = create_class "Signal"

function Signal:construct()
	self.wakelist = { }
end

function Signal:notify()
	local waketask = table.remove(self.wakelist, 1)
	if not waketask then
		return false
	end
	
	waketask:remove_task_observer(self)
	waketask:resume()
	return true
end

function Signal:notify_all()
	if #self.wakelist == 0 then
		return false
	end

	for _, task in ipairs(self.wakelist) do
		task:remove_task_observer(self)
		task:resume()
	end
	self.wakelist = { }
	return true
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
		return true
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

