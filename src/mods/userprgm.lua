local task = require "cbclua.task"
local cbc = require "cbclua.cbc"
local table = require "table"
local os = require "os"
local debug = require "debug"

-- Predeclares

local reset_interaction

-- Main program --

local main_task
local stop_hooks = { }

function add_stop_hook(hook)
	table.insert(stop_hooks, hook)
	return #stop_hooks
end

function remove_stop_hook(num)
	table.remove(stop_hooks, num)
end

function is_running()
	if main_task and main_task:get_state() ~= "stopped" then
		return true
	else
		return false
	end
end

function run()
	if is_running() then
		return false
	end
	
	reset() -- unload all modules so we start from a clean slate
	
	return pcall(function ()
		local mainmod = require "main"
		
		local function mainwrapper()
			mainmod.main()
			os.exit()
		end
		
		main_task = task.start(mainwrapper, "main")
		print("<<< Program running >>>")
	end)
end

function stop(who)
	if who == "exit" then
		print("<<< Program exited >>>")
	elseif is_running() then
		print("<<< Program stopped by " .. (who or "unknown") .. " >>>")
	end

	for _, hook in ipairs(stop_hooks) do
		pcall(hook, who)
	end
	
	task.stop_all_user_tasks()
	cbc.stop()
	return true
end

function reset()
	stop("reset")
	cbclua_unload_all_codemods()
	stop_hooks = { }
	collectgarbage("collect")
	reset_interaction()
end

-- Interaction --

local interact_mod
local interact_mod_loaded
local interact_mod_errmsg

function reset_interaction()
	interact_mod = { }
	cbclua_make_module(interact_mod)
	
	autorequire("", interact_mod) -- autorequire top level modules
	autorequire("cbclua.", interact_mod) -- and cbclua modules
	
	local ok, msg = pcall(function ()
		import("interact", interact_mod)
	end)
	
	interact_mod_loaded = ok
	
	if not ok and not msg:match("module 'interact' not found:") then
		interact_mod_errmsg = msg
	else
		interact_mod_errmsg = nil
	end
end

reset_interaction()

function is_interact_module_loaded()
	return interact_mod_loaded, interact_mod_errmsg
end

local interact_traceback

function interact(expr)
	local stacktrace = false
	if expr:match("^:t ") then
		stacktrace = true
		expr = expr:sub(4)
	end

	-- compile as both a bare chunk and as an expression to be returned
	local chunk, err = loadstring(expr, "=chunk") 
	local exprchunk = loadstring("return " .. expr, "=expr")
	
	local printdone = chunk ~= nil -- print done if we compiled as a chunk
	
	if exprchunk then -- if we're able to compile as an expression
		chunk = exprchunk -- evaluate the expression version to get a return value
	end
	
	if chunk == nil then -- if we've got no chunk to execute
		return false, err -- fail
	end
	
	setfenv(chunk, interact_mod)
	local resulttable 
	if stacktrace then
		resulttable = { xpcall(chunk, interact_traceback) }
	else
		resulttable = { pcall(chunk) }
	end
	local ok = resulttable[1]
	if not ok then
		return false, resulttable[2]
	end
	
	local resultstr
	if printdone and #resulttable == 1 then
		resultstr = "Done"
	else
		local resultparts = { }
		for i=2, #resulttable do
			local result = resulttable[i]
			local resultpart
			
			if type(result) == "string" then
				resultpart = "\"" .. result .. "\""
			else
				resultpart = tostring(result)
			end
			
			table.insert(resultparts, resultpart)
		end
		
		resultstr = table.concat(resultparts, ", ")
	end
	
	return true, resultstr
end

function interact_traceback(msg)
	local traceback = debug.traceback(msg, 3)
	local lines = { }
	for line in traceback:gmatch("(.-)\n") do
		table.insert(lines, line)
	end
	
	return table.concat(lines, "\n", 1, #lines-2)
end

-- 

function os.exit()
	stop("exit")
end

