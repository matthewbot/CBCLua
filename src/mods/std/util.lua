module("std.util")

local task = require "std.task"
local cbc = require "std.cbc"
local table = require "table"
local io = require "io"

--[[ System functions ]]--

function get_host() -- pc or chumby
	return _G.host
end

function get_mode() -- interact or console
	return _G.mode
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
