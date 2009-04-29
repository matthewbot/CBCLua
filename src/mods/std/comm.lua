module(...)

-- Requires and misc starting stuff --

local task = require 'std.task'
local timer = require 'std.timer'
local util = require 'std.util'
local string = require 'string'
local math = require 'math'
local io = require 'io'

-- Define SerialComm class

local SerialComm = create_class "SerialComm" -- local so ppl can't make their own
local serial_instance = nil -- holds the singleton instance

function SerialComm:construct()
	if serial_instance ~= nil then
		error("Only one SerialComm can created at a time!")
	end

	self.readbuf = ""
	self.rx = assert(io.open("/tmp/uart1rx", "rb"), "failed to open uart1rx")
	self.tx = assert(io.open("/tmp/uart1tx", "wb"), "failed to open uart1tx")
	self.readsig = task.Signal()
	self.readtask = task.start(util.bind(SerialComm.read_task, self), "serial read")
	
	serial_instance = self
end

function SerialComm:close()
	task.stop(self.task)
	self.tx:close()
	self.rx:close()
	serial_instance = nil -- clear the singleton holder so we can be recreated
end

function SerialComm:get_avail()
	return #self.readbuf
end

function SerialComm:check(amt)
	return #self.readbuf > amt
end

function SerialComm:clear()
	self.readbuf = ""
end

function SerialComm:wait_amt(amt)
	while #self.readbuf < amt do
		self.readsig:wait()
	end
end

function SerialComm:wait(amt, timeout)
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
		self.readsig:wait()
	end
		
	return true
end

function SerialComm:drop(amt)
	self.readbuf = self.readbuf:sub(amt+1)
end

function SerialComm:read(amt)
	self:wait_amt(amt)
	local data = self.readbuf:sub(1, amt)
	self:drop(amt)
	return data
end

function SerialComm:skip(amt)
	self:wait(amt)
	self:drop(amt)
end

function SerialComm:unread(data) 
	self.readbuf = data .. self.readbuf
end

function SerialComm:write(data, ...)
	if type(data) == "number" then -- if we've been given number arguments
		data = string.char(data, ...) -- make them into a string first
	end
	
	self.tx:write(data)
end

function SerialComm:read_task()
	while true do
		task.sleep_io(self.rx)
		self.readbuf = self.readbuf + file:read(9999)
	end
end

-- Utility functions for big-endian 16 bit words (create)

function get16(data, pos) -- reads a 16 bit value of the string data at position pos
	local high, low = string.byte(data, pos, pos+1)
	return high*256 + low -- no bit fiddling in lua since everything is a double internally
end

function make16(val) -- returns a new string containing the first 16 bits of val
	local high = math.floor(val / 256) -- again, sans bit fiddling
	local low = val % 256
	
	return string.char(high, low)
end

