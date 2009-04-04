module("main")

local task = require("std.task")
local util = require("std.util")

local taskdone = false

function taskfunc()
	for ctr=1,10 do
		print("Task Ctr: ", ctr)
		task.sleep(.3)
	end
	
	taskdone = true
end

function main() 
	print("Hello world!")
	
	local taskid = task.new(taskfunc)
	
	for ctr=20,0,-1 do
		print("Main Ctr: ", ctr)
		task.sleep(.1)
	end
	
	util.wait_for(function ()
		return taskdone
	end)
	
	print("Done")
end

