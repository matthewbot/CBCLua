local raw = require "cbclua.rawcbc"
local task = require "cbclua.task"

--[[ Globals ]]--

motors = { }
sensors = { }
servos = { }
buttons = { } -- buttons are also stored as globals (buttons["left"] == left_button)

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
	return raw.digital(self.num)
end

local CBCAnalogSensor = create_class("CBCAnalogSensor", SensorBase)

function CBCAnalogSensor:construct(num)
	self.num = num
end

function CBCAnalogSensor:read()
	return raw.analog10(self.num)
end

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

local function make_motor_wrapper(funcname)
	local func = raw[funcname]
	return function(self, ...) -- thunk replaces motor object with motor number
		return func(self.num, ...)
	end
end

-- these map directly
local motorfuncnames = { "fd", "bk", "off", "mav", "mtp", "mrp", "getpwm", "setpwm", "set_pid_gains" }
for _,funcname in ipairs(motorfuncnames) do
	Motor[funcname] = make_motor_wrapper(funcname)
end

-- these are renamed
Motor.getpos = make_motor_wrapper("get_motor_position_counter")
Motor.clearpos = make_motor_wrapper("clear_motor_position_counter")
Motor.getdone = make_motor_wrapper("get_motor_done")

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

Servo.__call = Servo.setpos -- possible shorthand, not sure if I like it yet

function Servo:getpos()
	return get_servo_position(self.num)
end

for i=1,4 do
	servos[i] = Servo(i)
end

enable_servos = enable_servos
disable_servos = disable_servos

--[[ Buttons ]]--

local Button = create_class "Button"

function Button:construct(name)
	self.btnfunc = raw[name .. "_button"]
	self.pressed = 0
end

function Button:press()
	self.pressed = self.pressed + 1
end

function Button:release()
	self.pressed = self.pressed - 1
end

function Button:__call()
	return self.pressed > 0 or self.btnfunc()
end

local buttonnames = { "black", "up", "down", "left", "right", "a", "b" }

for _,name in pairs(buttonnames) do
	local button = Button(name)
	_M[name .. "_button"] = button
	buttons[name] = button
end

--[[ Misc ]]--
power_level = power_level

--[[ Control functions ]]--

local dimlevel_proc = "/proc/sys/sense1/dimlevel"

function dim_screen()
	set_proc(dimlevel_proc, 1)
end

function bright_screen()
	set_proc(dimlevel_proc, 0)
end

function off_screen()
	set_proc(dimlevel_proc, 2)
end

function set_proc(file, val)
	if cbclua_get_host() ~= "chumby" then
		return
	end
	
	local file = io.open(file, "w")
	file:write(val, "\n")
	file:close()
end

