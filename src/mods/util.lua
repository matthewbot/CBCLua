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
	print(msg)
	print("Press A to continue")
	cbc.a_button:wait()
end

--[[ Menu ]]--

local menu_buttons = { cbc.up_button, cbc.down_button, cbc.left_button, cbc.right_button, cbc.a_button, cbc.b_button }

function menu(opts)
	local ctr=1
	local preds = { }
	
	for label,val in pairs(opts) do
		local button = menu_buttons[ctr]
		preds[val] = button
		print(button:getLetter() .. " " .. label)
		ctr = ctr + 1
	end
	
	print("Press a button")
	local val, button = task.wait_any(preds)
	task.wait_while(button) -- wait until button is released
	
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

