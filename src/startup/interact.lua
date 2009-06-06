local task = require "std.task"

if coroutine.coco == nil then
print[[
No CoCo in this lua interpreter.
This means you cannot call any
blocking task functions from within
an interaction command.
----------]]
end

local interact
local read_chunk
local run_chunk
local print_results
local print_errors

local env = { } -- make up a module to run our code in
env._M = env
env._NAME = "interact-env"
cbcluamodule(env)
autoglobals(env) -- allow globals to be created
autorequire("std.", env) -- automatically require the standard modules
autorequire("", env) -- automatically require top-level program modules
pcall(import, "interact", env) -- attempt to import the interact module into the env module
function env._WRAPVALUES(...) return ... end

function interact()
	io.writeln("Interaction started")
	
	while true do
		local chunk 
		repeat
			chunk = read_chunk()
		until chunk ~= nil
		
		local ok, results = run_chunk(chunk)
		if ok then
			if #results > 0 then
				print_results(results)
			end
		else
			print_errors(results)
		end
	end
end

function read_chunk()
	local lines = ""
	
	while true do
		if lines == "" then
			io.write("> ")
		else
			io.write(">> ")
		end
		
		task.sleep_io(io.stdin)
		
		local line = io.read()
		
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
			chunk, msg = loadstring("return _WRAPVALUES(" .. lines .. ")", "=stdin")
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
	lines = { }
	for line in string.gmatch(trace, "(.-)\n") do
		table.insert(lines, line)
	end

	for i=1,#lines-3 do
		io.writeln(lines[i])
	end
end
		
task.start(interact, "interaction", false, true)
