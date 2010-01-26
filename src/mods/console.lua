local task = require "cbclua.task"
local cbc = require "cbclua.cbc"
local maintask = require "cbclua.maintask"

local function console_task()
	while true do
		cbc.black_button:wait()
		
		if not maintask.is_running() then
			maintask.run()
		else
			task.stop_all_user_tasks()
		end
	end
end

function start_task()
	task.start(console_task, "console", false, true)
end
