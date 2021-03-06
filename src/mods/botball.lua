--- Botball starting light and timing utilities
module("cbclua.botball")

local util = require "cbclua.util"
local cbc = require "cbclua.cbc"
local timer = require "cbclua.timer"
local task = require "cbclua.task"
local io = require "io"
local os = require "os"

-- Private state

local start_time
local match_length

-- Predeclares

local wait_for_light
local shutdown_task
local assert_match_start

-- Functions

--- Gets the current game time
-- @return the number of seconds since the match started
function get_game_time()
	assert_match_start()
	
	return timer.seconds() - start_time
end

--- Gets the amount of game time remaining
-- @return number of seconds till the game ends
function get_game_time_remaining()
	assert_match_start()

	return match_length - game_game_time()
end

--- Sleeps until the specified time in game time
function game_time_sleep(gametime)
	assert_match_start()
	
	local amt = gametime - get_game_time()
	if amt > 0 then
		task.sleep(amt)
	end
end

--- Starts the game clock
function start_game_time()
	start_time = timer.seconds()
end

--- Botball game starting function
-- Handles the starting light as well as the shutdown timing
-- @param light_sensor a sensor object to use as the starting light
-- @param time the amount of time before the match terminates. Defaults to 110.
function start(light_sensor, time)
	match_length = time or 135

	print("Starting options")
	local option = util.menu{
		["Quick starting light"] = "quick",
		["Manual starting light"] = "manual",
		["Run immediately"] = "run"
	}
	
	if option == "quick" then
		wait_for_light_fast(light_sensor)
	elseif option == "manual" then
		wait_for_light(light_sensor)
	else
		util.wait_continue("Starting!")
	end
	
	print("Match started")
	start_game_time()
	task.start(shutdown_task, "shutdown task")
end

-- Helpers

function wait_for_light_fast(light)
	local THRESH = 500
	
	print("Turn light on")
	
	task.wait(function () return light() < THRESH end)
	
	print("Light ok, press A for hands off")
	while true do
		cbc.a_button:wait()
		if light() > THRESH then
			break
		end
		print("Light still on! Press A for hands off")
	end
	print("Waiting!")
	
	task.wait(function () return light() < THRESH end)
end

function wait_for_light(light)
	print("Beginning light calibration")
	
	util.wait_continue("Turn light on")
	
	local on = light:read()
	print("Light on: " .. on)
	
	util.wait_continue("Turn light off")
	
	local off = light:read()
	print("Light off: " .. off)
	
	local mid = (on + off) / 2
	print("Middle: " .. mid)
	
	print("Waiting for game start")
	task.wait(function () return light:read() < mid end)
end

function shutdown_task()
	task.sleep(match_length - 1) -- safety
	
	print("Game Over")
	os.exit()
end

function assert_match_start()
	if start_time == nil then
		error("Match hasn't started yet!", 3)
	end
end
