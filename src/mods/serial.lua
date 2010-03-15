module("cbclua.serial")

local task = require 'cbclua.task'
local timer = require 'cbclua.timer'
local util = require 'cbclua.util'
local rawio = require 'cbclua.rawio'
local string = require 'string'
local io = require 'io'

-- Define SerialPort class

SerialPort = create_class "SerialPort"
local weaktab = setmetatable({ }, { __mode = 'v' })

function SerialPort:construct()
	if weaktab.serial_instance ~= nil then -- if the weak table still has a reference to us
		collectgarbage("collect") -- do a full garbage collection to be sure
		if weaktab.serial_instance ~= nil then 
			error("Only one SerialPort can created at a time!", 2)
		end
	end

	self.readbuf = ""
	self.rx = assert(io.open("/dev/uart1", "rb"), "failed to open uart1 for reading") -- open read write so that we never get EOFS since we 
	self.tx = assert(io.open("/dev/uart1", "wb"), "failed to open uart1 for writing")
	self.tx:setvbuf("no")
	self.readsig = task.Signal()
	self.readtask = task.start(util.bind(self, "read_task"), "serial read")
	
	weaktab.serial_instance = self
end

function SerialPort:close()
	self.readtask:stop()
	self.tx:close()
	self.rx:close()
	weaktab.serial_instance = nil -- clear the singleton holder so we can be recreated
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
	if timeout then
		local curtime = timer.seconds()
		local stoptime = curtime + timeout
		while #self.readbuf < amt and curtime < stoptime do
			self.readsig:wait(stoptime - curtime)
			curtime = timer.seconds()
		end
		
		if curtime >= stoptime then
			return false
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
		self.readbuf = self.readbuf .. rawio.read(self.rx)
		self.readsig:notify_all()
	end
end
