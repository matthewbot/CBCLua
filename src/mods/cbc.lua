module("cbclua.cbc")

local raw = require "cbclua.rawcbc"
local task = require "cbclua.task"
local io = require "io"
local math = require "math"

--[[ SensorBase ]]--
-- Base class for any kind of sensor-looking thing

SensorBase = create_class "SensorBase"

function SensorBase:__call()
	return self:read()
end

function SensorBase:read()
	return self:read_raw()
end

function SensorBase:read_raw()
	error("Abstract")
end

--[[ CBCSensor ]]--
-- Private class representing raw CBC sensors

DigitalSensor = create_class("DigitalSensor", SensorBase)

function DigitalSensor:construct(num)
	self.num = num
end

function DigitalSensor:read_raw()
	return raw.digital(self.num)
end

AnalogSensor = create_class("AnalogSensor", SensorBase)

function AnalogSensor:construct(num)
	self.num = num
end

function AnalogSensor:read_raw()
	return raw.analog10(self.num)
end

--[[ Motors ]]--

Motor = create_class "Motor"

function Motor:construct(num)
	self.num = num
end

function Motor:wait()
	while not(raw.get_motor_done(self.num)) do
		task.sleep(0.020)
	end
end

-- Produces a wrapper method that substitutes the motor object first argument for the motor number
local function make_motor_wrapper(funcname)
	local func = raw[funcname]
	return function(self, ...) 
		return func(self.num, ...)
	end
end

-- these map directly
for _,funcname in ipairs{"fd", "bk", "off", "mav", "mtp", "mrp", "getpwm", "setpwm", "set_pid_gains"} do
	Motor[funcname] = make_motor_wrapper(funcname)
end

-- these are renamed
Motor.getpos = make_motor_wrapper("get_motor_position_counter")
Motor.clearpos = make_motor_wrapper("clear_motor_position_counter")
Motor.getdone = make_motor_wrapper("get_motor_done")

--[[ Servos ]]--

Servo = create_class "Servo"

function Servo:construct(num)
	self.num = num
end

function Servo:setpos(pos)
	if pos <= 0 or pos >= 2047 then
		error("Attempt to set servo position out of range (must be 0-2047)", 2)
	end

	raw.set_servo_position(self.num, pos)
end

function Servo:__call(pos) 
	self:setpos(pos) -- possible shorthand, not sure if I like it yet
end

function Servo:getpos()
	return raw.get_servo_position(self.num)
end

enable_servos = raw.enable_servos
disable_servos = raw.disable_servos

--[[ Buttons ]]--

Button = create_class "Button"

function Button:construct(name)
	self.name = name
	self.btnfunc = raw[name .. "_button"]
	self.pressed = 0
end

-- used by interact to remotely simulate a button press
function Button:press()
	self.pressed = self.pressed + 1
end

function Button:release()
	self.pressed = self.pressed - 1
end

function Button:getPressed()
	return self.pressed > 0 or self.btnfunc()
end

function Button:__call()
	return self:getPressed()	
end

function Button:wait()
	return task.wait_toggle(self)
end

function Button:getLetter()
	return self.name:sub(1,1):upper()
end

--[[ Misc ]]--
power_level = raw.power_level
display_clear = raw.display_clear
beep = raw.beep

function stop()
	for i=0,3 do
		raw.off(i)
	end
	
	raw.disable_servos()
end

--[[ Control functions ]]--

local brightness_proc = "/proc/sys/sense1/brightness"

function dim_screen()
	set_screen_brightness(.1)
end

function bright_screen()
	set_screen_brightness(1)
end

function off_screen()
	set_screen_brightness(0)
end

function set_screen_brightness(brightval)
	set_proc(brightness_proc, math.floor(brightval * 65535 + .5))
end

function set_proc(file, val)
	if cbclua_get_host() ~= "chumby" then
		return
	end
	
	local file = assert(io.open(file, "w"), "Failed to open proc '" .. file .. "'")
	file:write(val, "\n")
	file:close()
end

