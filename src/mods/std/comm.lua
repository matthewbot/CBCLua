module(...)

-- Requires and misc starting stuff --

local serial = require 'raw.serial'
local task = require 'std.task'
local timer = require 'std.timer'
local string = require 'string'
local math = require 'math'

-- Put a new sleep function in the task schedular that uses serial.poll to wake 
-- the process up when new serial data arrives

task.set_sleep_func(function (time)
	timer.watchdog_disable()
	if serial.poll(time) then
		return { serial = true }
	else
		return { }
	end
end)

-- Define SerialComm class

local SerialComm = create_class "SerialComm" -- local so ppl can't make their own
local serial_instance = nil -- holds the singleton instance

function open() -- this is how one obtains a SerialComm
	if serial_instance then
		error("Only one instance of SerialComm can exist!")
	end
	
	serial_instance = SerialComm()
	return serial_instance
end

function SerialComm:construct()
	self.readbuf = ""
	serial.init()
end

function SerialComm:close()
	self.readbuf = ""
	serial.quit()
	
	serial_instance = nil -- clear the singleton holder so we can be recreated
end

function SerialComm:fill_buf(timeout)
	local canread = serial.poll(0) -- find if we can perform a nonblocking read right now
	
	if not(canread) then -- if we can't
		if timeout == 0 then return end -- and non blocking, then terminate now
	
		if timeout > 0 then -- if we can timeout
			local gotevent = task.sleep_event("serial", timeout) -- tell the task system to wait for the serial event or our timeout
			
			if not(gotevent) then return end -- if we got timed out, terminate now
		else -- no timeout
			task.sleep_event("serial") -- tell the task system to pause us indefinitely until the serial event
		end
	end
	
	self.readbuf = self.readbuf + serial.read()
end

function SerialComm:get_avail()
	self:fill_buf()
	return #self.readbuf
end

function SerialComm:check(amt)
	if #self.readbuf >= amt then
		return true
	else
		self:fill_buf()
		return #self.readbuf >= amt
	end
end

function SerialComm:clear()
	repeat
		self.readbuf = ""
		self:fill_buf()
	until self.readbuf == ""
end

function SerialComm:wait_amt(amt)
	while #self.readbuf < amt do
		self:fill_buf(-1) -- wait indefinitely
	end
end

function SerialComm:wait(amt, timeout)
	local start = timer.seconds()
	
	while #self.readbuf < amt do
		local remaining = timeout - (timer.seconds() - start)
	
		if remaining <= 0 then
			return false
		end
		
		self:fill_buf(remaining)
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
	self:wait_amt(amt)
	self:drop(amt)
end

function SerialComm:unread(data) 
	self.readbuf = data .. self.readbuf
end

function SerialComm:write(data, ...)
	if type(data) == "number" then -- if we've been given number arguments
		data = string.char(data, ...) -- make them into a string first
	end
	
	serial.write(data)
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

