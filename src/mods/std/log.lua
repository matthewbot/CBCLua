module(...)

local timer = require "std.timer"
local task = require_later "std.task"
local io = require "io"
local string = require "string"
local table = require "table"

--

local logfile = io.open("log", "w")
local time_func = timer.seconds

function set_time_func(func) -- used to change log timestamps to use botball time
	log("new time function set")
	time_func = func
end

local function log_header(src)
	local taskname = task.get_name(task.get_current())
	if taskname == nil then
		taskname = "system"
	end
	return string.format(">>> [%06.2f] <%s> %s:\n", time_func(), taskname, src)
end

local function get_module(depth)
	depth = depth or 2
	
	return getfenv(depth+1)._NAME or "unknown"
end

function log(disp, src, msg)
	local header

	if src == nil then -- log(msg)
		msg = disp
		src = get_module()
		disp = false
	elseif msg == nil then -- log(disp, msg)
		msg = src
		src = get_module()
	end
	
	local header = log_header(src)
	logfile:write(header, msg, "\n")
	
	if disp then
		if src ~= "print" then
			io.write(src, ": ")
		end
		
		io.write(msg, "\n")
	end
end

function _G.print(...)
	local vals = { }
	for num = 1,select('#', ...) do
		local val = select(num, ...)
		table.insert(vals, tostring(val))
	end
	
	return log(true, "print", table.concat(vals))
end
