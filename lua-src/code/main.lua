module("main")

local cbc = require "std.cbc"
local debug = require "debug"

function main() 
	cbc.motors[2]:fd()
	cbc.motors[1]:mtp(1000, -500)
	
	cbc.servos[2]:setpos(400)
	cbc.enable_servos()
end

