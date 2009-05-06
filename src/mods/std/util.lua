module("std.util")

--[[ Waiting functions ]]--

local task = require "std.task"
local cbc = require "std.cbc"
local table = require "table"
local io = require "io"

function wait(pred, time) 
	if time == nil then
		time = 0.050
	end

	while not(pred()) do
		task.sleep(time)
	end
end

function wait_greater(pred, thresh, time)
	wait(function ()
		return pred() > thresh
	end, time)
end

function wait_less(pred, thresh, time)
	wait(function ()
		return pred() < thresh
	end, time)
end

function wait_any(args, time)
	if time == nil then
		time = 0.050
	end
	
	while true do
		for name,pred in pairs(args) do
			if pred() then
				return name
			end
		end
		
		task.sleep(time)
	end
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

--[[ User interaction functions ]]--

function prompt(msg)
	io.writeln(msg, " (press A)")
	wait(cbc.a_button)
end

function wait_continue(msg)
	io.writeln(msg)
	io.writeln("Press A to continue")
	wait(cbc.a_button)
end
