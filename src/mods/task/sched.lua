local timer = require "cbclua.timer"
local table = require "table"

local current_task = nil

function get_current_task()
	return current_task
end

--[[ TaskEntry lists ]]--

local active_list = { } -- TaskEntrys currently running
local active_list_nextpos = 1
local sleep_list = { } -- TaskEntrys currently sleeping, ordered by wake up time
local io_list = { } -- TaskEntrys currently waiting on IO

local function search_list(list, val)
	local pos=1
	while pos <= #list do
		if list[pos] == val then
			return pos
		else
			pos = pos + 1
		end
	end
end

function add_active_task(task)
	table.insert(active_list, task)
end

local debug = require "debug"

function remove_active_task(task, noerr)
	local pos = search_list(active_list, task)

	if not noerr then
		assert(pos, "Can't remove task that isn't active!")
	elseif pos == nil then
		return
	end
	
	table.remove(active_list, pos)
	if active_list_nextpos > pos then -- adjust the next position
		active_list_nextpos = active_list_nextpos - 1
	end
end

function add_sleep_task(task)
	local waketime = task.waketime
	assert(waketime ~= nil, "Task missing waketime field!")
	
	local pos=1
	while pos <= #sleep_list do
		if sleep_list[pos].waketime >= waketime then
			break
		end
		
		pos = pos + 1
	end
	
	table.insert(sleep_list, pos, task)
end

function remove_sleep_task(task)
	local pos = assert(search_list(sleep_list, task), "Can't remove task that isn't sleeping!")
	
	table.remove(sleep_list, pos)
end

function add_io_task(task)
	assert(task.wakeio, "Task missing wakeio field!")
	table.insert(io_list, task)
end

function remove_io_task(task)
	local pos = assert(search_list(io_list, task), "Can't remove task that isn't blocking on IO!")
	
	table.remove(io_list, pos)
end

--[[ Scheduler ]]--

local run_active_tasks
local wake_sleep_tasks
local block_io

function run()
	while true do
		wake_sleep_tasks()
		run_active_tasks()
		
		local sleepamt
		if #active_list > 0 then
			sleepamt = 0
		elseif #sleep_list > 0 then
			sleepamt = sleep_list[1].waketime - timer.seconds()
		elseif #io_list > 0 then
			sleepamt = -1 -- indefinite sleep
		else
			error("deadlock") -- theres apparently no code available to run, so we have to crash
		end
		
		block_io(sleepamt)
	end
end

function wake_sleep_tasks()
	local curtime = timer.seconds()

	while #sleep_list > 0 do
		local toptask = sleep_list[1]
		if toptask.waketime <= curtime then
			remove_sleep_task(toptask)
			toptask:sched_wakeup()
		else
			break -- list is sorted so all tasks after this one have later wake up times
		end
	end
end

function run_active_tasks()
	active_list_nextpos = 1
	while active_list_nextpos <= #active_list do
		local curpos = active_list_nextpos
		active_list_nextpos = active_list_nextpos + 1
		
		local task = active_list[curpos]
		
		current_task = task
		local running = task:sched_resume()
		current_task = nil
		
		if not running then
			if task.errmsg then
				print("Error in task '" .. task.name .. "'\n" .. task.errmsg)
			end
			remove_active_task(task, true)
		end
		
		assert(active_list_nextpos > 0 and active_list_nextpos <= #active_list+1, "active_list_nextpos out of range")
	end
end

function block_io(sleepamt)
	local files = { }
	for i,iotask in ipairs(io_list) do
		files[i] = iotask.wakeio
	end

	local wakeflags = { timer.sleep_select(sleepamt, unpack(files)) }
	
	local waketasks = { }
	for i,task in ipairs(io_list) do
		if wakeflags[i] then
			table.insert(waketasks, task)
		end
	end
	
	for _,waketask in ipairs(waketasks) do
		remove_io_task(waketask)
		waketask:sched_wakeup_io()
	end
end

