local task = require "cbclua.task"
local cbc = require "cbclua.cbc"

local main_task

function is_running()
	return main_task and main_task:running()
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
	if not is_running() then
		return false
	end

	task.stop_all_user_tasks()
	cbc.stop()
	print("<<< Program stopped by " .. (who or "unknown") .. " >>>")
	return true
end


