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

function on_cbc_console() -- true if we're on a CBC in console mode, false if not (usually the only thing that matters)
	return get_host() == "cbc" and get_mode() == "console"
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

function wait_enter(fmt)
	if on_cbc_console() then
		error("Can't call wait_enter on the cbc console!")
	end
	
	task.sleep_io(io.stdin)
	return io.read(fmt or "*l")
end

function prompt(msg)
	io.write(msg)
	if on_cbc_console() then
		io.writeln(" (press A)")
		task.wait(cbc.a_button)
	else
		io.write(" (push Enter)")
		wait_enter()
	end
end

function wait_continue(msg)
	io.writeln(msg)
	if on_cbc_console() then
		io.writeln("Press A to continue")
		task.wait(cbc.a_button)
	else
		io.write("Press Enter to continue")
		wait_enter()
	end
end

--[[ Other functions ]]--

function bind(obj, methname)
	local func = obj[methname]
	return function (...)
		return func(obj, ...)
	end
end

function after_do(func, endfunc)
	local returns = { func() }
	endfunc(unpack(returns))
	return unpack(returns)
end
