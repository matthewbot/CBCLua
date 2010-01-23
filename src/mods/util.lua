local task = require "cbclua.task"
local cbc = require "cbclua.cbc"
local table = require "table"
local io = require "io"

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

function wait_continue(msg)
	io.writeln(msg)
	io.writeln("Press A to continue")
	task.wait(cbc.a_button)
	task.wait_while(cbc.a_button)
end

--[[ Menu ]]--

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

