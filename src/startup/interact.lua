if coroutine.coco == nil then
cbclua_log([[
No CoCo in this lua interpreter.
This means you cannot call any
blocking task functions from within
an interaction command]])
end

local task = require "std.task"

local interact
local run_command
local run_chunk

function interact()
	local env = { }
	env._M = env
	env._NAME = "interact"
	cbcluamodule(env)
	autoglobals(env)
	autorequire(env)
	autorequire("", env)
	
	print("cbclua: interaction started")
	while true do
		run_command(env)
	end
end

function run_command(env)
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
				print("command canceled")
			end
			return
		end
		
		line = line:gsub("^local ", "")
		lines = lines .. line .. " "
		local chunk, msg = loadstring("return " .. lines, "=stdin")
		if chunk == nil then
			chunk, msg = loadstring(lines, "=stdin")
		end
		
		if chunk then
			setfenv(chunk, env)
			local rets = { select(2, run_chunk(chunk)) }
			if #rets > 0 then
				print(unpack(rets))
			end
			return
		end
		
		if not(msg:find("<eof>", 1, true)) then
			print(msg)
			return
		end
	end
end

function run_chunk(chunk)
	return xpcall(
		chunk
	, function (errmsg)
		local trace = debug.traceback(errmsg, 2)
		
		lines = { }
		for line in string.gmatch(trace, "(.-)\n") do
			table.insert(lines, line)
		end
		
		local out = ""
		
		for i=1,#lines-3 do
			if out == "" then
				out = lines[i]
			else
				out = out .. "\n" .. lines[i]
			end
		end
		
		print(out)
	end)
end
		
task.start(interact, "interaction", false, true)
