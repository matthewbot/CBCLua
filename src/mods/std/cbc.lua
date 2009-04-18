module("std.cbc")

-- /proc/sys/sense1/brightness

--[[ Requires and globals ]]--

local class = require "std.class"
local raw = require "raw.cbc"

global{"sensors", "motors", "servos"}

--[[ Sensors ]]--

local Sensor = class.create("Sensor")

function Sensor:construct(num, type)
	self.num = num
	self.type = type
end

function Sensor:get_type()
	return self.type
end

function Sensor:read()
	if self.type == "analog" then
		return raw.analog10(self.num)
	else
		return raw.digital(self.num) == 1 -- convert to a bool
	end
end

function Sensor:read_float()
	if self.type == "analog" then
		return raw.analog10(self.num)/1024
	else
		error("Attempting to read a digital sensor as a floating value", 2)
	end
end

Sensor.__call = Sensor.read -- Calling a sensor reads it

sensors = { } -- array of sensors
for i=0,7 do
	sensors[i] = Sensor(i, "digital")
end
for i=8,15 do
	sensors[i] = Sensor(i, "analog")
end

--[[ Motors ]]--

local Motor = class.create("Motor")

function Motor:construct(num)
	self.num = num
end

local function make_motor_wrapper(rawfuncname, realfuncname)
	if realfuncname == nil then
		realfuncname = rawfuncname
	end
	
	local func = raw[rawfuncname]

	return function(self, ...)
		return raw.__errwrap(2, realfuncname, func, self.num, ...) -- errwrap makes error messages come out correctly since some functions are renamed
	end
end

local motorfuncnames = { "fd", "bk", "off", "mav", "mtp", "mrp" }
for _,funcname in ipairs(motorfuncnames) do
	Motor[funcname] = make_motor_wrapper(funcname)
end

Motor.wait = make_motor_wrapper("bmd", "wait")
Motor.getpos = make_motor_wrapper("get_motor_position_counter", "getpos")
Motor.clearpos = make_motor_wrapper("clear_motor_position_counter", "clearpos")
Motor.getdone = make_motor_wrapper("get_motor_done", "getdone")

motors = { }
for i=0,3 do
	motors[i] = Motor(i)
end

--[[ Servos ]]--

local Servo = class.create("Servo")

function Servo:construct(num)
	self.num = num
end

function Servo:setpos(pos)
	if pos <= 0 or pos >= 2047 then
		error("Attempt to set servo position out of range (must be 0-2047)", 2)
	end

	raw.set_servo_position(self.num, pos)
end

function Servo:getpos()
	return raw.get_servo_position(self.num)
end

servos = { }
for i=1,4 do
	servos[i] = Servo(i)
end

enable_servos = raw.enable_servos
disable_servos = raw.disable_servos

--[[ Buttons ]]--

local buttons = { "black", "up", "down", "left", "right" }

for _,button in ipairs(buttons) do
	local btnfunc = button .. "_button" -- all button functions end in _button
	_M[btnfunc] = raw[btnfunc]
end
