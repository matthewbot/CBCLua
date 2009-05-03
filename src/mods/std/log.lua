module(...)

local timer = require "std.timer"
local io = require "io"
local string = require "string"

local logfile = io.open("log", "w")

function log(...)
	logfile:write(string.format("[%06.2f] ", timer.seconds()))
	
	local first=true
	for _,val in ipairs{...} do
		if first then
			first = false
		else
			logfile:write("\t")
		end
		logfile:write(val)
	end
	
	logfile:write("\n")
end

local realprint = print
function _G.print(...)
	realprint(...)
	log(...)
end

log("log: started")
