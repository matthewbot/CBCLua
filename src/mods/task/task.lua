local timer = require "cbclua.timer"
local coroutine = require "coroutine"
local debug = require "debug"

-- Task class

Task = create_class "Task"

function Task:construct(func, name, cstack, system)
	self.co = coroutine.create(func, cstack and 0 or -1)
	self.name = name
	self.system = system
end

function Task:get_name()
	return self.name
end

function Task:is_system()
	return self.system == true
end

function Task:running()
	if self.co then
		return coroutine.status(self.co) ~= "dead"
	else
		return false
	end
end

-- returns ok, result/msg,
-- ok is true if the task resumed and returned with no error, false if an error aborted the task
-- result is the table yielded from a control function, or nil of the task ended
-- msg is the error the task halted with
function Task:resume()
	assert(current_task == nil, "Can't resume task from within a task!")
	assert(self:running(), "Can't resume a task that isn't running!")

	local co = self.co
	
	local ok,result = coroutine.resume(co)
	
	if ok then
		if self:running() then	
			assert(result ~= nil, "Task can't yield nil!")
			return true, result
		else
			return true, nil -- must return nil if we ended, we don't care what the task function returned as a value
		end
	else
		local msg = debug.traceback(co, "error in task '" .. self.name .. "':\n" .. result)
		return false, msg
	end
end

-- Do not call this! Call task.stop() instead.
function Task:kill()
	self.co = nil
end

