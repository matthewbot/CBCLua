module("cbclua.cbc")

local raw = require "cbclua.rawcbc"
local task = require "cbclua.task"
local bit = require "cbclua.bit"
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

function DigitalSensor:construct(args)
	self.num = assert(args.num or args[1], "Missing sensor num argument to DigialSensor")
	self.flip = args.flip
end

function DigitalSensor:read_raw()
	return raw.digital(self.num)
end

function DigitalSensor:read()
	local val = self:read_raw()
	if self.flip then
		val = not val
	end
	return val
end

AnalogSensor = create_class("AnalogSensor", SensorBase)

function AnalogSensor:construct(args)
	self.num = assert(args.num or args[1], "Missing sensor num argument to AnalogSensor")
	if args.float ~= nil then
		self:set_float(args.float)
	else
		self:set_float(false)
	end
end

function AnalogSensor:set_float(val)
	if val == nil then val = true end
	
	local bits = raw.get_analog_floats()
	if val then
		bits = bit.set(bits, self.num)
	else
		bits = bit.clear(bits, self.num)
	end
	raw.set_analog_floats(bits)
end

function AnalogSensor:get_float()
	return bit.get(raw.get_analog_floats(), self.num)
end

function AnalogSensor:read_raw()
	return raw.analog10(self.num)
end

--[[ Motors ]]--

Motor = create_class "Motor"

function Motor:construct(args)
	self.num = assert(args.num or args[1], "Missing num argument to Motor")
	if args.pid then
		self:set_pid(args.pid)
	end
end

function Motor:wait()
	while not(raw.get_motor_done(self.num)) do
		task.sleep(0.020)
	end
end

local default_pid = {p=3, i=3/2, d=-2/3}
function Motor:set_pid(args)
	local p = args.p or default_pid.p
	local i = args.i or default_pid.i
	local d = args.d or default_pid.d
	local denom = args.denom or 30
	raw.set_pid_gains(self.num, math.round(p*denom), math.round(i*denom), math.round(d*denom), denom, denom, denom)
end

-- Produces a wrapper method that substitutes the motor object first argument for the motor number
local function make_motor_wrapper(funcname)
	local func = raw[funcname]
	return function(self, ...) 
		return func(self.num, ...)
	end
end

-- these map directly
for _,funcname in ipairs{"fd", "bk", "off", "mav", "mtp", "mrp", "getpwm", "setpwm"} do
	Motor[funcname] = make_motor_wrapper(funcname)
end

-- these are renamed
Motor.getpos = make_motor_wrapper("get_motor_position_counter")
Motor.clearpos = make_motor_wrapper("clear_motor_position_counter")
Motor.getdone = make_motor_wrapper("get_motor_done")

--[[ Servos ]]--

Servo = create_class "Servo"

function Servo:construct(args)
	self.num = assert(args.num or args[1], "Missing servo num argument to Servo")
end

function Servo:setpos(pos)
	if pos < 0 or pos > 2047 then
		error("Attempt to set servo position out of range (must be 0-2047)", 2)
	end

	raw.set_servo_position(self.num, pos)
end

function Servo:disable()
	raw.set_servo_position(self.num, -1)
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
local button_pressed = { }

function Button:construct(name)
	self.name = name
	self.btnfunc = raw[name .. "_button"]
end

function Button:getPressed()
	if self.btnfunc() then return true end
	local presscount = button_pressed[self.name]
	if presscount ~= nil and presscount >= 1 then return true end
	return false
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

function Button:press()
	press_button(self.name)
end

function Button:release()
	release_button(self.name)
end

function press_button(name)
	local presscount = button_pressed[name]
	if presscount == nil then
		button_pressed[name] = 1
	else
		button_pressed[name] = presscount + 1
	end
end

function release_button(name)
	local presscount = button_pressed[name]
	if presscount == nil or presscount == 0 then
		return
	else
		button_pressed[name] = presscount - 1
	end
end

for _, name in pairs{"up","down","left","right","a","b","black"} do
	_M[name .. "_button"] = Button(name)
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

