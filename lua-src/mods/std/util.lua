module("std.util")

local task = require "std.task"

function wait(pred) 
	while not(pred()) do
		task.yield()
	end
end

function wait_greater(pred, thresh)
	wait(function ()
		return pred() > thresh
	end)
end

function wait_less(pred, thresh)
	wait(function ()
		return pred() < thresh
	end)
end
	
