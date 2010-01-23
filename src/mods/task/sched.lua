module(...)

local list = require "cbclua.task.list"
local timer = require "cbclua.timer"
local os = require "os"
local math = require "math"
local table = require "table"

-- Private state

local file_set = { }
local wake_all_flag = false

-- Predeclares

local run_cycle
local run_sleep

-- Public functions

function get_current()
	return current_task
end

function wake_all()
	wake_all_flag = true
end

function check_io_flag(file)
	return file_set[file] == true
end

function run()
	collectgarbage("collect") -- do a full garbage collection since we accumulate stuff loading the program
	collectgarbage("stop") -- than stop the automatic collector

	while true do
		local endtime, files = run_cycle()
	
		if wake_all_flag then
			endtime = 0
			wake_all_flag = false
		end
	
		file_set = run_sleep(endtime, files)
	end
end

-- Private functions

function run_cycle()
	local minendtime = math.huge
	local files = { }
	local files_set = { }

	for task in list.running_tasks() do
		list.set_current_task(task)
		local ok, result = task:resume()
		list.set_current_task(nil)
		
		if ok then
			if result then
				local endtime = result.endtime
				if endtime and endtime < minendtime then
					minendtime = endtime
				end
		
				local file = result.file
				if file and not files_set[file] then
					files_set[file] = true
					table.insert(files, file)
				end
			else
				list.stop(task)
			end
		else -- not ok
			print(result) -- error message
			list.stop(task)
		end
	end
	
	if list.start_new_tasks() then -- if any new tasks were started
		wake_all() -- we need to wake up all tasks again so that they get started and sleep information gets recomputed
	end
	
	return minendtime, files
end

function run_sleep(endtime, files) 
	local sleepamt
	
	if endtime == math.huge then
		sleepamt = -1
	else
		sleepamt = endtime - timer.seconds()
		if sleepamt < 0 then
			sleepamt = 0
		end
	end
	
	if sleepamt ~= 0 then -- if we're about to go to sleep
		collectgarbage("step") -- perform an incremental gc step
		collectgarbage("stop") -- have to re-stop it again
		
		if sleepamt ~= -1 then	
			sleepamt = endtime - timer.seconds()
			if sleepamt < 0 then
				sleepamt = 0
			end
		end
	end
	
	if sleepamt == -1 and #files == 0 then
		print("Deadlock detected, ending program")
		os.exit(1)
	end
	
	local bools = { timer.sleep_select(sleepamt, unpack(files)) }
	local file_set = { }
	
	for num,file in ipairs(files) do
		if bools[num] then
			file_set[file] = true
		end
	end
	
	return file_set
end
	
