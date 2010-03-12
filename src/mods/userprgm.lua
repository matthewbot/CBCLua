local task = require "cbclua.task"
local cbc = require "cbclua.cbc"
local table = require "table"
local os = require "os"
local io = require "io"
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
	
	cbc.stop()
	task.stop_all_user_tasks()

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

function load_interaction()
	reset_interaction()
end

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
	local exprchunk 
	if not expr:match("^import") then -- hack for import statements to work correctly
		exprchunk = loadstring("return " .. expr, "=expr")
	end
	
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
	if #resulttable == 1 then
		if printdone then
			resultstr = "Done"
		else
			resultstr = "nil"
		end
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

-- USB loading

local mount_usb_script = "/mnt/kiss/usercode/mount-usb"
local unmount_usb_script = "/mnt/kiss/usercode/umount-usb"
local usb_path = "/mnt/browser/usb/"

function usb_load()
	if cbclua_get_host() ~= "chumby" then
		error("user_load() only implemented on chumby", 2)
	end

	if os.execute(mount_usb_script) ~= 0 then
		return false, "Failed to mount USB"
	end
	
	local usbload = io.open(usb_path .. "usbload.txt", "r")
	if not usbload then
		os.execute(unmount_usb_script)
		return false, "Missing usbload.txt"
	end
	
	local load_path = usbload:read()
	usbload:close()
	local code_path = cbclua_get_codepath()
	
	if os.execute("rm -rf " .. code_path) ~= 0 then
		os.execute(unmount_usb_script)
		return false, "Failed to remove old code"
	end
	
	if os.execute("cp -r " .. usb_path .. load_path .. " " .. code_path) ~= 0 then
		os.execute(unmount_usb_script)
		return false, "Failed to copy loadpath (" .. load_path .. ") to code directory"
	end
	
	os.execute("sync")
	
	if os.execute(unmount_usb_script) ~= 0 then
		return false, "Failed to unmount usb drive"
	end
	
	return true, "Loaded " .. load_path
end

function usb_load_verbose()
	print("<<< Loading program from USB >>>")
	local ok, msg = usb_load()
	if ok then
		if msg then
			print("<<< " .. msg .. " >>>")
		else
			print("<<< Program loaded! >>>")
		end
	else
		print("!!! Program failed to load !!!")
		
		if msg then
			print("!!! " .. msg .. " !!!")
		end
	end
end
-- 

function os.exit()
	stop("exit")
end

