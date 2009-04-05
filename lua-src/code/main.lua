module("main")

local cbc = require "std.cbc"
local task = require "std.task"

function blah()
	error("Foo?")
end

function main() 
	cbc.motors[2]:fd()
	cbc.motors[1]:mtp(2, 100)
	
	cbc.servos[2]:setpos(400)
	cbc.enable_servos()
	
	task.sleep(3)
end



