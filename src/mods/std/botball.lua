module(...)

local util = require "std.util"
local cbc = require "std.cbc"
local timer = require "std.timer"
local task = require "std.task"
local io = require "io"
local os = require "os"

import "std.log"

-- Private state

local start_time
local match_length

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

	return match_length - game_game_time()
end

function start(light_sensor, time)
	match_length = time or 120

	local option = util.menu{
		["Use starting light"] = "light",
		["Run immediately"] = "run"
	}
	
	if option == "light" then
		wait_for_light(light_sensor)
	else
		if util.on_cbc_console() then
			print("Starting light skipped, starting in 3 seconds")
			task.sleep(3)
		else
			util.prompt("Starting signal")
		end
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
	
	print("Waiting for game start")
	task.wait(function () return light:read() < mid end)
end

function shutdown_task()
	task.sleep(match_length - 0.5) -- safety
	
	print("Game Over")
	os.exit()
end

function assert_match_start()
	if start_time == nil then
		error("Match hasn't started yet!", 3)
	end
end
