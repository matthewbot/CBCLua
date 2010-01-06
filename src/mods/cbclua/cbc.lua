module(...)

-- /proc/sys/sense1/brightness

--[[ Requires and globals ]]--

import "raw.cbc"
local task = require "cbclua.task"
local util = require "cbclua.util"

--[[ SensorBase ]]--
-- Base class for any kind of sensor-looking thing

SensorBase = create_class "SensorBase"

function SensorBase:__call()
	return self:read()
end

--[[ CBCSensor ]]--
-- Private class representing raw CBC sensors

local CBCDigitalSensor = create_class("CBCDigitalSensor", SensorBase)

function CBCDigitalSensor:construct(num)
	self.num = num
end

function CBCDigitalSensor:read()
	return digital(self.num)
end

local CBCAnalogSensor = create_class("CBCAnalogSensor", SensorBase)

function CBCAnalogSensor:construct(num)
	self.num = num
end

function CBCAnalogSensor:read()
	return analog10(self.num)
end

sensors = { } -- array of sensors
for i=0,7 do
	sensors[i] = CBCDigitalSensor(i)
end
for i=8,15 do
	sensors[i] = CBCAnalogSensor(i)
end

--[[ Motors ]]--

local Motor = create_class "Motor"

function Motor:construct(num)
	self.num = num
end

function Motor:wait()
	while not(get_motor_done(self.num)) do
		task.sleep(0.020)
	end
end

local function make_motor_wrapper(rawfuncname, realfuncname)
	if realfuncname == nil then
		realfuncname = rawfuncname
	end
	
	local func = _M[rawfuncname] -- this will still search our imports

	return function(self, ...)
		return __errwrap(2, realfuncname, func, self.num, ...) -- errwrap makes error messages come out correctly since some functions are renamed
	end
end

local motorfuncnames = { "fd", "bk", "off", "mav", "mtp", "mrp", "getpwm", "setpwm", "set_pid_gains" }
for _,funcname in ipairs(motorfuncnames) do
	Motor[funcname] = make_motor_wrapper(funcname)
end

Motor.getpos = make_motor_wrapper("get_motor_position_counter", "getpos")
Motor.clearpos = make_motor_wrapper("clear_motor_position_counter", "clearpos")
Motor.getdone = make_motor_wrapper("get_motor_done", "getdone")

motors = { }
for i=0,3 do
	motors[i] = Motor(i)
end

--[[ Servos ]]--

local Servo = create_class "Servo"

function Servo:construct(num)
	self.num = num
end

function Servo:setpos(pos)
	if pos <= 0 or pos >= 2047 then
		error("Attempt to set servo position out of range (must be 0-2047)", 2)
	end

	set_servo_position(self.num, pos)
end

function Servo:getpos()
	return get_servo_position(self.num)
end

servos = { }
for i=1,4 do
	servos[i] = Servo(i)
end

enable_servos = enable_servos
disable_servos = disable_servos

--[[ Buttons ]]--

local buttons = { "black", "up", "down", "left", "right", "a", "b" }

for _,button in ipairs(buttons) do
	local btnfunc = button .. "_button" -- all button functions end in _button
	_M[btnfunc] = _M[btnfunc] -- looks weird, but on the right we're going to find it in our import, and then on the left we're going to make it a module variable
end

--[[ Control functions ]]--

local dimlevel_proc = "/proc/sys/sense1/dimlevel"

function dim_screen()
	util.set_cbc_proc(dimlevel_proc, 1)
end

function bright_screen()
	util.set_cbc_proc(dimlevel_proc, 0)
end

function off_screen()
	util.set_cbc_proc(dimlevel_proc, 2)
end

