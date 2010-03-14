local task = require "cbclua.task"
local cbc = require "cbclua.cbc"
local userprgm = require "cbclua.userprgm"

local function console_task()
	while true do
		task.wait(cbc.black_button)
		local released = task.wait_while(cbc.black_button, 1.0)
		
		if released then
			if not userprgm.is_running() then
				local ok, msg = userprgm.run()
				
				if not ok then
					print("!!! Failed to run main !!!")
					print(msg)
				end
			else
				userprgm.stop("console")
			end
		else
			userprgm.usb_load_verbose()
			
			local ok, msg = userprgm.is_interact_module_loaded()
			
			if not ok and msg then
				print("!!! Error while loading interaction module !!!")
				print(msg)
			end
			
			task.wait_while(cbc.black_button)
		end
	end
end

function start_task()
	task.start(console_task, "console", "system")
end

