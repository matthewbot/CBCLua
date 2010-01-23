local task = require "cbclua.task"
local cbc = require "cbclua.cbc"
local os = require "os"

local main_task

function cbclua_is_main_running()
	return main_task and main_task:running()
end

function cbclua_run_main()
	if cbclua_is_main_running() then
		return false
	end
	
	return pcall(function ()
		local mainmod = require "main"
		main_task = task.start(mainmod.main, "main")
		io.write("<<< Program running >>>\n")
	end)
end

function cbclua_stop_main()
	if cbclua_is_main_running() then
		task.stop(main_task)
		io.write("<<< Program stopped >>>\n")
	end
	
	main_task = nil
end

local realexit
function os.exit(...) -- map os.exit calls from a code module to cbclua_stop_main
	if cbclua_is_codemod(getfenv(2)) then
		return cbclua_stop_main()
	else
		return realexit(...)
	end
end

local function console_task()
	while true do
		task.wait(cbc.black_button)
		task.wait_while(cbc.black_button)
		if cbclua_is_main_running() then
			cbclua_run_main()
		else
			cbclua_stop_main()
		end
	end
end

task.start(console_task, "console", false, true)

