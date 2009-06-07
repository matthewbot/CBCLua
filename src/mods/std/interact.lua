module(...)

-- Setup code

local util = require "std.util"
local task = require "std.task"
local io = require "io"
local debug = require "debug"
local table = require "table"
local string = require "string"

if util.get_mode() ~= "interact" then -- if we're not in interact mode
	function run() end -- declare a run no-op
	return -- and return
end

-- Predeclares

local read_chunk
local run_chunk
local print_results
local print_errors
local commands = { }

-- Interact module setup
-- this creates a module dynamically to hold the interaction within

local env = { _NAME = "interact-env" } -- make up a module to run our code in
env._M = env
cbcluamodule(env) -- make it a cbclua module
autoglobals(env) -- allow globals to be created
autorequire("std.", env) -- automatically require the standard modules

local ok, msg = xpcall(function () -- attempt to load an interact customization module
	import("interact", env)
end, function (errmsg)
	if errmsg:find("module 'interact' not found") then 
		return "notfound"
	else
		return debug.traceback(errmsg, 2)
	end
end)

if not ok and msg ~= "notfound" then
	io.writeln("Error while loading interact customization module")
	io.writeln(msg)
	io.writeln("interaction customization disabled")
end

function env._WRAPVALUES(...) return ... end

-- Module state

local interact_depth=0

-- Interact loop

function run()
	interact_depth = interact_depth + 1

	local prevchunk
	while true do
		local chunk 
		repeat
			chunk = read_chunk()
			
			if chunk == "repeat" then
				chunk = prevchunk
			end
		until chunk ~= nil
		
		if chunk == "exit" then
			task.exit()
		end
		
		if chunk == "run" then
			break
		end
		
		local ok, results = run_chunk(chunk)
		if ok then
			if #results > 0 then
				print_results(results)
			end
		else
			print_errors(results)
		end
		
		prevchunk = chunk
	end
	
	interact_depth = interact_depth - 1
end

function read_chunk()
	local lines = ""
	
	while true do
		if interact_depth ~= 1 then
			io.write(interact_depth)
		end
	
		if lines == "" then
			io.write("> ")
		else
			io.write(">> ")
		end
		
		task.sleep_io(io.stdin)
		local line = io.read() or ":exit"
		
		if line == "" and lines == "" then
			return "repeat"
		end
		
		local command = line:match("^:(%w+)")
		if command then
			if command == "exit" or command == "quit" or command == "q" then return "exit" end
			if interact_depth > 1 and (command == "continue" or command == "c") then return "run" end
			
			local func = commands[command]
			if func then
				return func
			else
				io.writeln("Unknown command")
				return nil
			end
		end
		
		if line == "" then
			if lines ~= "" then
				io.writeln("Command canceled")
			end
			
			return nil
		end
		
		line = line:gsub("^local ", "")
		lines = lines .. line .. "\n"
		
		local chunk, msg 
		
		if not line:match("^%s*import") then
			chunk, msg = loadstring("return _WRAPVALUES(" .. lines .. ")", "=stdin") -- defeats tail call gumming up the error msg
		end
		
		if chunk == nil then
			chunk, msg = loadstring(lines, "=stdin")
		end
		
		if chunk then
			return chunk
		end
		
		if not(msg:find("<eof>", 1, true)) then
			io.writeln(msg)
			lines = ""
		end
	end
end

function run_chunk(chunk)
	setfenv(chunk, env)
	local results = {xpcall(
		chunk
	, function (errmsg)
		return debug.traceback(errmsg, 2)
	end)}
	
	local ok = table.remove(results, 1)
	
	if ok then
		return ok, results
	else
		return ok, results[1] -- results[1] will be the error msg
	end
end

function commands.run()
	if interact_depth ~= 1 then
		io.writeln("Already running")
	else
		local main = require "main"
		main.main()
	end
end

function commands.trace()
	local debug = require "debug"
	local lines = { }
	for line in debug.traceback():gmatch("(.-)\n") do
		table.insert(lines, line)
	end
	
	lines[2] = ""
	lines[#lines] = ""
	lines[#lines-1] = ""
	
	i = #lines-2
	while i >= 1 do
		local line = lines[i]
		if line:match("mods/std/interact%.lua.+run") then
			lines[i] = ""
			lines[i-1] = ""
			lines[i-2] = ""
			i = i-3
		else
			i = i-1
		end
	end
	
	for _,line in ipairs(lines) do
		if line ~= "" then
			io.writeln(line)
		end
	end
end
	

function print_results(results)
	for num,v in ipairs(results) do
		if num > 1 then
			io.write(", ")
		end
	
		io.write(tostring(v))
	end

	io.write("\n")
end

function print_errors(trace)
	local lines = { }
	for line in string.gmatch(trace, "(.-)\n") do
		table.insert(lines, line)
	end

	for i=1,#lines-3 do
		io.writeln(lines[i])
	end
end
