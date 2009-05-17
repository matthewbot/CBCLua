module(...)

local util = require "std.util"
local cbc = require "std.cbc"
local timer = require "std.timer"
local task = require "std.task"
local io = require "io"

import "std.log"

-- Private state

local start_time
local match_time

-- Predeclares

local wait_for_light
local shutdown_task
local assert_match_start

-- Functions

function get_game_time()
	assert_match_start()
	
	return timer.seconds() - start_time
end

function get_game_time_remaining()
	assert_match_start()

	return match_time - game_game_time()
end

function wait_for_start(light_sensor, time)
	if time == nil then
		time = 120
	end

	match_time = time

	io.writeln("Press A to calibrate starting light\npress Left to skip and run program")
	local option = util.wait_any{
		light = cbc.a_button,
		skip = cbc.left_button
	}
	
	if option == "light" then
		wait_for_light(light_sensor)
	else
		print("Starting light skipped, starting in 3 seconds")
		task.sleep(3)
	end
	
	print("Match started")
	start_time = timer.seconds()
	set_time_func(get_game_time) -- this makes the logging system report the time in game time
	task.start(shutdown_task, "shutdown task", true)
end

-- Helpers

function wait_for_light(light)
	print("Beginning light calibration")
	
	util.prompt("Turn light on")
	
	local on = light:read()
	print("Light on: " .. on)
	
	util.prompt("Turn light off")
	
	local off = light:read()
	print("Light off: " .. off)
	
	local mid = (on + off) / 2
	print("Middle: " .. mid)
	
	io.writeln("Turn light off")
	util.wait_less(light, mid)
	
	print("Waiting for game start")
	util.wait_greater(light, mid)
end

function shutdown_task()
	task.sleep(match_time - 0.5) -- safety
	
	task.terminate("Game Over")
end

function assert_match_start()
	if start_time == nil then
		error("Match hasn't started yet!", 3)
	end
end
