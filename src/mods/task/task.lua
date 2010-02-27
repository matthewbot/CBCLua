local sched = require "cbclua.task.sched"
local timer = require "cbclua.timer"
local coroutine = require "coroutine"
local debug = require "debug"

Task = create_class("Task", sched.TaskEntry)

-- Task Sets --

local user_task_set = setmetatable({ }, {__mode="k"})
local all_task_set = setmetatable({ }, {__mode="k"})

function user_tasks()
	return pairs(user_task_set)
end

function all_tasks()
	return pairs(all_task_set)
end

local next_task_id = 1
function Task:construct(func, name, typestr)
	assert(type(func) == "function", "Function argument (2) must be a function!")
	assert(type(name) == "string", "Name argument (1) must be string!")
	self.func = func
	self.name = name
	self.id = next_task_id
	next_task_id = next_task_id + 1

	if typestr then
		self.system = typestr:find("system", 1, true)
		self.cstack = typestr:find("cstack", 1, true)
	end
	
	all_task_set[self] = true
	if not self.system then
		user_task_set[self] = true
	end
	
	self.state = "stopped"
	self.stateobservers = { }
end

function Task:get_id()
	return self.id
end

function Task:get_name()
	return self.name
end

function Task:get_state()
	return self.state
end

function Task:is_system()
	return self.system
end

function Task:set_state(state)
	local prevstate = self.state
	
	if prevstate == state then
		return
	end
	
	self.state = state
	self:notify_task_observers(prevstate)
end

function Task:notify_task_observers(prevstate)
	local state = self.state
	
	local new_stateobservers = { } -- could be more efficient, data structure library would help here too
	for observer, _ in pairs(self.stateobservers) do
		local keep = observer:taskobserver_state_changed(self, state, prevstate)
		if keep then
			new_stateobservers[observer] = true
		end
	end
	
	self.stateobservers = new_stateobservers
end

function Task:add_task_observer(observer)
	self.stateobservers[observer] = true
end

function Task:remove_task_observer(observer)
	self.stateobservers[observer] = false
end

function Task:get_error()
	return self.errmsg
end

function Task:start()
	if self.state ~= "stopped" then
		return
	end
	
	self.co = coroutine.create(self.func, self.cstack)
	self.errmsg = nil
	sched.add_active_task(self)
	self:set_state("active")
end

function Task:stop()
	if self.state == "active" then
		sched.remove_active_task(self)
	elseif self.state == "sleep" then
		sched.remove_sleep_task(self)
	elseif self.state == "sleepio" then
		sched.remove_io_task(self)
	end
	
	self:set_state("stopped")
	self.co = nil
	
	if sched.get_current_task() == self then
		coroutine.yield()
	end
end

function Task:suspend()
	if self.state ~= "active" then
		error("Can't suspend inactive task!", 2)
	end
	
	self:set_state("suspended")
	sched.remove_active_task(self)
	
	if sched.get_current_task() == self then
		coroutine.yield()
	end
end

function Task:resume()
	if self.state ~= "suspended" and self.state ~= "sleep" then
		error("Can't resume task that isn't suspended or sleeping!", 2)
	end
	
	if self.state == "sleep" then
		self.waketime = nil
	end
	self:set_state("active")
	sched.add_active_task(self)
end
	
function Task:sleep(amt)
	return self:sleep_till(amt + timer.seconds())
end
	
function Task:sleep_till(waketime)
	assert(sched.get_current_task() == self, "Can't sleep a task that isn't the current task!")
	
	self.waketime = waketime
	self:set_state("sleep")
	
	sched.remove_active_task(self)
	sched.add_sleep_task(self)
	return coroutine.yield()
end

function Task:sleep_io(io)
	assert(sched.get_current_task() == self, "Can't sleep_io a task that isn't the current task!")
	
	self.wakeio = io
	self:set_state("sleepio")
	
	sched.remove_active_task(self)
	sched.add_io_task(self)
	return coroutine.yield()
end

function Task:yield()
	assert(sched.get_current_task() == self, "Can't yield a task that isn't the current task!")
	
	return coroutine.yield()
end

-- scheduler callbacks

function Task:sched_resume()
	local co = self.co
	assert(co and self.state == "active", "Sched can't resume inactive Task!")
	
	local ok, errmsg = coroutine.resume(co)
	
	if coroutine.status(co) ~= "dead" then
		return true
	else
		if not ok then
			self.errmsg = debug.traceback(co, errmsg)
		end
		
		self:set_state("stopped")
		
		return false
	end
end
	
function Task:sched_wakeup()
	assert(self.state == "sleep", "Sched can't wake up non-sleeping Task!")
	
	self:set_state("active")
	self.waketime = nil
	sched.add_active_task(self)
end

function Task:sched_wakeup_io()
	assert(self.state == "sleepio", "Sched can't wake-io a non-iosleeping Task!")
	
	self:set_state("active")
	self.wakeio = nil
	sched.add_active_task(self)	
end

