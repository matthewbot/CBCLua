module("cbclua.sched.activelist")

local timer = require "cbclua.timer"
local table = require "table"

local current_task = nil

function get_current_task()
	return current_task
end

local active_list = { } -- TaskEntrys currently running
local active_list_nextpos = 1

function add_task(task, resumearg)
	task:clear_wait_type()
	if resumearg then
		task:set_resume_arg(resumearg)
	end
	table.insert(active_list, task)
end

function remove_task(task, waittype)
	if waittype then
		task:set_wait_type(waittype)
	end

	local pos = table.findvalue(active_list, task)
	if not pos then return end
	
	table.remove(active_list, pos)
	if active_list_nextpos > pos then -- adjust the next position
		active_list_nextpos = active_list_nextpos - 1
	end
end

function get_count()
	return #active_list
end

function get_current()
	return current_task
end

--[[ main funcs ]]--

local function get_next_task()
	local curpos = active_list_nextpos
	active_list_nextpos = active_list_nextpos + 1
	return active_list[curpos]
end

function run()
	active_list_nextpos = 1
	while active_list_nextpos <= #active_list do
		local task = get_next_task()
		
		current_task = task
		task:resume()
		current_task = nil
		
		if task:get_state() == "stopped" then
			local err = task:get_error()
			if err then
				print("Error in task '" .. task:get_name() .. "'\n" .. err)
			end
			
			remove_task(task)
		end
		
		assert(active_list_nextpos > 0 and active_list_nextpos <= #active_list+1, "active_list_nextpos out of range")
	end
end

