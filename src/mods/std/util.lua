module("std.util")

local task = require "std.task"
local cbc = require_later "std.cbc"
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
	return get_host() == "chumby" and get_mode() == "console"
end

print(_G.host, _G.mode)

	
--[[ Table functions ]]--

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
		task.wait(function () return not cbc.a_button() end)
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
		task.wait(function () return not cbc.a_button() end)
	else
		io.write("Press Enter to continue")
		wait_enter()
	end
end

--[[ Menu ]]--

if on_cbc_console() then

	local button_names = { "U", "D", "L", "R", "A", "B" }
	local button_funcs = { cbc.up_button, cbc.down_button, cbc.left_button, cbc.right_button, cbc.a_button, cbc.b_button }
	
	function menu(opts)
		local ctr=1
		local preds = { }
		
		for label,val in pairs(opts) do
			preds[val] = button_funcs[ctr]
			io.writeln(button_names[ctr] .. " " .. label)
			ctr = ctr + 1
		end
		
		io.writeln("Press a button")
		
		local val = task.wait_any(preds)
		task.wait(function () return not preds[val]() end)
		return val
	end
	
else

	function menu(opts)
		local ctr=1
		local vals = { }
		
		for label,val in pairs(opts) do
			vals[ctr] = val
			io.writeln(ctr .. " " .. label)
			ctr = ctr + 1
		end
		
		io.writeln("Press a number, then enter")
		
		local num = wait_enter("*l")+0 -- +0 converts str to int
		return vals[num]
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

function set_cbc_proc(file, val)
	if get_host() ~= "chumby" then
		return
	end
	
	local file = io.open(file, "w")
	file:write(val, "\n")
	file:close()
end
