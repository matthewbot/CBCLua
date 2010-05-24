module("cbclua.sched.taskentry")

local coroutine = require "coroutine"
local debug = require "debug"
local table = require "table"

local all_task_set = setmetatable({}, { __mode = "k" })
local user_task_set = setmetatable({}, { __mode = "k" })

TaskEntry = create_class("TaskEntry")

local next_task_id = 1
local function get_task_id()
	local id = next_task_id
	next_task_id = next_task_id + 1
	return id
end

function TaskEntry:construct(func, name, typestr)
	assert(type(func) == "function", "Function argument (2) must be a function!")
	assert(type(name) == "string", "Name argument (1) must be string!")
	
	self.name = name
	self.id = get_task_id()
	self.cleanup_funcs = { }
	
	local cstack
	if typestr then
		self.system = typestr:find("system", 1, true)
		cstack = typestr:find("cstack", 1, true)
	else
		self.system = false
		cstack = false
	end
	
	self.co = coroutine.create(func, cstack and 0 or -1)
	
	all_task_set[self] = true
	if not self.system then
		user_task_set[self] = true
	end
end

function TaskEntry:get_id()
	return self.id
end

function TaskEntry:get_name()
	return self.name
end

function TaskEntry:get_type()
	return self.system and "system" or "user"
end

function TaskEntry:get_state()
	if self.co then
		if self.wait_type then
			return "wait"
		else
			return "active"
		end
	else
		return "stopped"
	end
end

function TaskEntry:get_wait_type()
	return self.wait_type
end

function TaskEntry:set_wait_type(wait_type)
	self.wait_type = wait_type
end

function TaskEntry:clear_wait_type()
	self.wait_type = nil
end

function TaskEntry:get_error()
	return self.errmsg
end

function TaskEntry:register_cleanup(func)
	table.insert(self.cleanup_funcs, func)
	return #self.cleanup_funcs
end

function TaskEntry:unregister_cleanup(id)
	table.remove(self.cleanup_funcs, id)
end

function TaskEntry:stop()
	if not self.co then return end
	
	for _, func in ipairs(self.cleanup_funcs) do
		func()
	end
	
	self.co = nil
end

function TaskEntry:set_resume_arg(arg)
	assert(self.resume_arg == nil, "Can't set multiple resume args on a TaskEntry!")
	self.resume_arg = arg
end

function TaskEntry:resume()
	local co = self.co
	assert(co, "Can't resume stopped TaskEntry '" .. self.name .. "' !")
	
	local ok, errmsg = coroutine.resume(co, self.resume_arg)
	self.resume_arg = nil
	
	if coroutine.status(co) == "dead" then
		self:stop()
		
		if not ok then
			errmsg = debug.traceback(co, errmsg)
			self.errmsg = errmsg
			return false, errmsg
		end
	end
	
	return true
end

function all_tasks() return table.keys(all_task_set) end
function user_tasks() return table.keys(user_task_set) end


