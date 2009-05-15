module(...)

local timer = require "std.timer"
local os = require "os"
local math = require "math"
local table = require "table"

import "std.task.list"

-- Private state

local file_set = { }
local wake_all_flag = false

-- Predeclares

local run_cycle
local run_sleep

-- Public functions

function wake_all()
	wake_all_flag = true
end

function check_io_flag(file)
	return file_set[file] == true
end

function run()
	while true do
		local endtime, files = run_cycle()
	
		if real_count() == 0 then
			return
		end
	
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

	for task in running_tasks() do
		local result = task:resume()
		local endtime = result.endtime
		local file = result.file
		
		if endtime and endtime < minendtime then
			minendtime = endtime
		end
			
		if file and files_set[file] == nil then
			files_set[file] = true
			table.insert(files, file)
		end	
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
	
	if sleepamt == -1 and #files == 0 then
		print("Deadlock detected, ending program")
		os.exit(1)
	end
	
	local bools = { timer.raw_sleep(sleepamt, unpack(files)) }
	local file_set = { }
	
	for num,file in ipairs(files) do
		if bools[num] then
			file_set[file] = true
		end
	end
	
	return file_set
end
	
