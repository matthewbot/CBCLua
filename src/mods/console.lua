local task = require "cbclua.task"
local cbc = require "cbclua.cbc"
local userprgm = require "cbclua.userprgm"

local function console_task()
	while true do
		cbc.black_button:wait()
		
		if not userprgm.is_running() then
			userprgm.run()
		else
			userprgm.stop("console")
		end
	end
end

function start_task()
	task.start(console_task, "console", "system")
end

