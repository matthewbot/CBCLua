module("std.util")

--[[ Waiting functions ]]--

local task = require "std.task"
local table = require "table"

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
	
--[[ Table functions ]]--

local string = require "string"

function findvalue(table, val)
	for k,v in pairs(table) do
		if v == val then
			return k
		end
	end
end

function deepclone(table)
	local newtable = { }
	for k,v in pairs(table) do
		if type(v) == "table" then
			newtable[k] = table.deepclone(v)
		else
			newtable[k] = v
		end
	end
	
	return newtable
end

function prettyprint(table, indent)
	indent = indent or 0
	local indent_str = string.rep(" ", indent*2)
	
	for k, v in pairs(table) do
		if type(v) == "table" then
			print(indent_str .. k .. ": ")
			prettyprint(v, indent+1)
		else
			print(indent_str .. k .. ": " .. tostring(v))
		end
	end
end

--[[ Misc functions ]]--

function bind(method, obj)
	return function (...)
		return method(obj, ...)
	end
end

function bindargs(method, ...)
	local arg = {...}
	return function (...)
		return method(unpack(table.concat(arg, {...}))) -- not particularly efficient
	end
end
