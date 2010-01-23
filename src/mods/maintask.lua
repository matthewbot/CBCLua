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
		main_task = task.start(mainmod.main, "main")
		print("<<< Program running >>>")
	end)
end

