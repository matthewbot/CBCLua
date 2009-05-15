module(...)

local timer = require "std.timer"
local coroutine = require "coroutine"
local debug = require "debug"

-- Private State --

local current_task

-- Task class

Task = create_class "Task"

function Task:construct(func, name, daemon, cstack)
	self.co = coroutine.create(func, cstack and 0 or -1)
	self.name = name
	self.daemon = daemon
end

function Task:get_name()
	return self.name
end

function Task:is_daemon()
	return self.daemon == true
end

function Task:resume()
	assert(current_task == nil, "Can't resume task from within a task!")

	local co = self.co
	
	current_task = self
	local ok,result = coroutine.resume(co)
	current_task = nil
	
	if ok then
		return result
	else
		local msg = debug.traceback(co, "error in task '" .. self.name .. "':\n" .. result)
		error(msg, 0)
	end
end

-- Public functions

function get_current()
	return current_task
end

