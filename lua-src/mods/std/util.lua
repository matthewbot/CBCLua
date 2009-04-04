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

-- Any errors that occur while running func appear to come from 2 levels higher in the stack
-- (IE, not the method calling passerrors but the method calling that one
function passerrors(func, ...)
	local results = { pcall(func, ...) } -- this captures every return value by pcall into a table
	if result[1] == false then
		error(result[2], 2)
	end
	
	return unpack(results, 2) -- return the results sans status code
end
	
