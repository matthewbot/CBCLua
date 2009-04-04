module("main")

local cbc = require("std.cbc")
local task = require("std.task")

function taskfunc()
	for ctr=1,10 do
		print("Task Ctr: ", ctr)
		task.sleep(.2)
	end
end

function main() 
	print("Hello world!")
	
	local taskid = task.new(taskfunc)
	
	for ctr=20,0,-1 do
		print("Main Ctr: ", ctr)
		task.sleep(.1)
	end
end

