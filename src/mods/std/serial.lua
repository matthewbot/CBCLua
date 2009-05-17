module(...)

-- Requires and misc starting stuff --

local task = require 'std.task'
local timer = require 'std.timer'
local util = require 'std.util'
local string = require 'string'
local io = require 'io'

import("std.log")

-- Define SerialPort class

SerialPort = create_class "SerialPort"
local serial_instance = nil -- holds the singleton instance

function SerialPort:construct()
	if serial_instance ~= nil then
		error("Only one SerialPort can created at a time!")
	end

	self.readbuf = ""
	self.rx = assert(io.open("/tmp/uart1rx", "rb"), "failed to open uart1rx")
	self.tx = assert(io.open("/tmp/uart1tx", "wb"), "failed to open uart1tx")
	self.rx:setvbuf("no")
	self.tx:setvbuf("no")
	self.readsig = task.Signal()
	self.readtask = task.start(util.bind(self, "read_task"), "serial read", true)
	
	serial_instance = self
	
	log("serial port opened")
end

function SerialPort:close()
	task.stop(self.readtask)
	self.tx:close()
	self.rx:close()
	serial_instance = nil -- clear the singleton holder so we can be recreated
	
	log("serial port closed")
end

function SerialPort:get_avail()
	return #self.readbuf
end

function SerialPort:check(amt)
	return #self.readbuf >= amt
end

function SerialPort:clear()
	self.readbuf = ""
end

function SerialPort:wait(amt, timeout)
	if timeout ~= nil then
		local start = timer.seconds()

		while #self.readbuf < amt do
			local remaining = timeout - (timer.seconds() - start)

			if remaining <= 0 then
				return false
			end
	
			self.readsig:wait(remaining)
		end
	else
		while #self.readbuf < amt do
			self.readsig:wait()
		end
	end
		
	return true
end

function SerialPort:drop(amt)
	self.readbuf = self.readbuf:sub(amt+1)
end

function SerialPort:read(amt)
	self:wait(amt)
	local data = self.readbuf:sub(1, amt)
	self:drop(amt)
	return data
end

function SerialPort:skip(amt)
	self:wait(amt)
	self:drop(amt)
end

function SerialPort:unread(data) 
	self.readbuf = data .. self.readbuf
end

function SerialPort:write(data, ...)
	if type(data) == "number" then -- if we've been given number arguments
		data = string.char(data, ...) -- make them into a string first
	end
	
	self.tx:write(data)
end

function SerialPort:read_task()
	while true do
		task.sleep_io(self.rx)
		self.readbuf = self.readbuf .. timer.raw_getio(self.rx)
		self.readsig:notify()
	end
end
