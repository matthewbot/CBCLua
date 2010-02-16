local task = require "cbclua.task"
local cbc = require "cbclua.cbc"
local table = require "table"
local os = require "os"

-- Main program --

local main_task
local shutdown_hooks = { }

function add_shutdown_hook(hook)
	table.insert(shutdown_hooks, hook)
end

function is_running()
	return main_task and main_task:get_state() ~= "stopped"
end

function run()
	if is_running() then
		return false
	end
	
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

	cbc.stop()
	for _, hook in ipairs(shutdown_hooks) do
		pcall(hook)
	end
	shutdown_hooks = { }
	
	task.stop_all_user_tasks()
	return true
end

function unload()
	cbclua_unload_all_codemods()
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

function interact(expr)
	-- compile as both a bare chunk and as an expression to be returned
	local chunk, err = loadstring(expr, "=chunk") 
	local exprchunk = loadstring("return " .. expr, "=expr")
	
	local printnil -- whether to print a nil value
	if chunk then -- if we compiled as a chunk
		printnil = false -- don't print a nil
	else
		printnil = true
	end
	
	if exprchunk then -- if we're able to compile as an expression
		chunk = exprchunk -- evaluate the expression version to get a return value
	end
	
	if chunk == nil then -- if we've got no chunk to execute
		return false, err -- fail
	end
	
	setfenv(chunk, interact_mod)
	local ok, result = pcall(chunk)
	if not ok then
		return false, result
	end
	
	local resulttype = type(result)
	local resultstr
	
	if resulttype == "string" then
		resultstr = "\"" .. result .. "\""
	elseif resulttype == "nil" then
		if printnil then
			resultstr = "nil"
		else
			resultstr = "Done"
		end
	else
		resultstr = tostring(result)
	end
	
	return true, resultstr
end

-- 

function os.exit()
	stop("exit")
end

