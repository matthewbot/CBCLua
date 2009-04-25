module("std.kissc")

local task = require "std.task"
local timer = require "std.timer"
local raw = require "raw.cbc"

-- define some common KISS-C functions in terms of the cbclua standard library

sleep = task.sleep
defer = task.yield
seconds = timer.seconds

-- Redefine block_motor_done to use task.sleep and co-operative multithreading instead of
-- putting the entire process to sleep

function block_motor_done(mot)
	while not(raw.get_motor_done(mot))
		sleep(0.020)
	end
end

bmd = block_motor_done

