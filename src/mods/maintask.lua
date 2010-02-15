local task = require "cbclua.task"
local cbc = require "cbclua.cbc"
local table = require "table"
local os = require "os"

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
	task.stop_all_user_tasks()
	cbc.stop()
	
	for _, hook in ipairs(shutdown_hooks) do
		pcall(hook)
	end
	shutdown_hooks = { }
	
	print("<<< Program stopped by " .. (who or "unknown") .. " >>>")
	return true
end

function os.exit()
	stop("os.exit")
end


