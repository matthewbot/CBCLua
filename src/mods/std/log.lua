module(...)

local timer = require "std.timer"
local task = require_later "std.task"
local io = require "io"
local string = require "string"
local os = require "os"

-- Private State --

local logname = "log" -- in current directory (more portable)
local logfile = io.tmpfile() -- start in the temporary log
local started = false

local time_func = timer.seconds

-- Predeclares

local get_module
local get_task
local log_file_header

-- Public Interface

function start()
	if started then return end

	local real_logfile = assert(io.open(logname, "w"), "failed to open log file for writing")

	logfile:seek("set")
	real_logfile:write(logfile:read("*a")) -- copy our temp log to the real log
	
	logfile:close()
	logfile = real_logfile -- switch to the real log
		
	logfile:flush()
	logfile:setvbuf("line") -- turn on line buffering, so if program hard crashes log won't be lost in a buffer
	
	started = true
end

function set_time_func(func) -- used to change log timestamps to use botball time
	log("new time function set")
	time_func = func
end

function log(msg, depth)
	local module = get_module((depth or 1) + 1)
	local time = time_func()
	local task = get_task()
	local header = log_file_header(time, task, module)

	logfile:write(header, msg, "\n")
end

function _G.print(msg, ...)
	local buf = tostring(msg)

	for i=1,select("#", ...) do
		local val = select(i, ...)
		
		buf = buf .. "\t" .. tostring(val)
	end
	
	log(buf, 2)
	io.writeln(buf)
end

-- Helper functions

function get_module(depth)
	local mod = getfenv((depth or 1) + 1)
	local modname

	if mod then
		modname = mod._NAME
	end
	
	modname = modname or "cbclua"
	
	return modname
end

function get_task()
	local curtask = task.get_current()
	
	if curtask then
		return curtask:get_name()
	else
		return "system"
	end

	return taskname or "system"
end
		
function log_file_header(time, task, module)
	return string.format(">>> [%06.2f] <%s> %s:\n", time, task, module)
end
		
