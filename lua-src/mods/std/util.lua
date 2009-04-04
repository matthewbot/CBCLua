module("std.util")

local task = require "std.task"

function wait_for(pred) 
	while not(pred()) do
		task.yield()
	end
end

function wait_greater(pred, thresh)
	wait_for(function ()
		return pred() > thresh
	end)
end

function wait_less(pred, thresh)
	wait_for(function ()
		return pred() < thresh
	end)
end
