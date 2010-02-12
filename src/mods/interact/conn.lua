local task = require "cbclua.task"
local interactenv = require "cbclua.interact.interactenv"
local rawio = require "cbclua.rawio"
local cbc = require "cbclua.cbc"
local maintask = require "cbclua.maintask"
local util = require "cbclua.util"
local os = require "os"
local io = require "io"
import "cbclua.interact.config"

local globalenv

InteractConnection = create_class "InteractConnection"

function InteractConnection:construct(sock, callback)
	self.sock = sock
	self.callback = callback
	self:init()
	self.task = task.start(util.bind(self, "run"), "interact conn", "system")
end

function InteractConnection:init()
	self.sock:settimeout(0)
	self:write_line(cbclua_get_name())
	self:write_line(cbclua_get_version())
end	

function InteractConnection:run()
	self:update_task_list()

	if globalenv == nil then
		self:make_globalenv()
	end
	
	while true do
		local command = self:read_line()
		
		if command == nil then
			break
		end
		
		local meth = self["cmd_" .. command:lower()]
		if meth then
			meth(self)
		else
			self:write_line("ERROR")
			self:write_data("Bad command " .. command)
		end
	end
		
	self.sock:close()
	self.callback.on_conn_close(self)
end

function InteractConnection:cmd_expr()
	local expr = self:read_data()
	task.start(function ()
		local ok, result = globalenv:run(expr)
		if ok then
			self:write_line("RESULT")
		else
			self:write_line("ERROR")
		end
		self:write_data(result)
	end, "interact eval", "cstack")
end

function InteractConnection:cmd_runmain()
	if maintask.is_running() then
		self:send_error("Program already running")
	else
		local ok, err = maintask.run()
	
		if not ok then
			self:send_error("Error while running main:\n" .. err)
		end
	end
end

function InteractConnection:cmd_buttondown()
	local buttonname = self:read_line()
	cbc.buttons[buttonname]:press()
end

function InteractConnection:cmd_buttonup()
	local buttonname = self:read_line()
	cbc.buttons[buttonname]:release()
end

function InteractConnection:cmd_stoptasks()
	if maintask.is_running() then
		maintask.stop("interact") -- go through the maintask mechanism when possible to print status messages
	else
		-- if the main task isn't technically running, kill all user tasks anyway, 
		-- might've made one using interaction
		task.stop_all_user_tasks()
		cbc.stop()
	end
end

function InteractConnection:cmd_stoptask()
	local taskid = tonumber(self:read_line())
	
	for t in task.all_tasks() do
		if t:get_id() == taskid then
			t:stop()
			break
		end
	end
end

function InteractConnection:cmd_clearcode()
	os.execute("rm -rf " .. cbclua_get_codepath() .. "/*")
	cbclua_unload_all_codemods()
end	

function InteractConnection:cmd_mkcodedir()
	local dir = self:read_line()
	rawio.mkdir(cbclua_get_codepath() .. "/" .. dir)
end

function InteractConnection:cmd_putcode()		
	local filename = self:read_line()
	local filedata = self:read_data()
	
	local file = io.open(cbclua_get_codepath() .. "/" .. filename, "w")
	if file then
		file:write(filedata)
		file:close()
	end
end

function InteractConnection:cmd_resetenv()
	self:make_globalenv()
end

function InteractConnection:make_globalenv()
	globalenv = interactenv.InteractEnvironment()
	
	local loaded, msg = globalenv:is_module_loaded()
	if not loaded and msg then
		self:send_error("Interact module not loaded:\n" .. msg)
	end
end

function InteractConnection:update_task_list()
	self:write_line("TASKLIST")
	for t in task.all_tasks() do
		self:write_line(t:get_id())
		self:write_line(t:get_name())
		self:write_line(t:get_state())
		self:write_line(t:is_system() and "system" or "normal")
	end
	self:write_line("")
end

function InteractConnection:send_print(data)
	self:write_line("PRINT")
	self:write_data(data)
end

function InteractConnection:send_error(err)
	self:write_line("ERROR")
	self:write_data(err)
end

function InteractConnection:write_line(line)
	self:send_all(line .. "\n")
end

function InteractConnection:write_data(data)
	self:write_line(data:len())
	self:send_all(data)
end
	
function InteractConnection:send_all(data)
	local sock = self.sock
	local pos = 0
	while pos ~= data:len() do
		local amt, err = sock:send(data, pos)
		if amt == nil then
			return false, err
		end
		pos = pos + amt
	end
end
	
function InteractConnection:read_line()
	local sock = self.sock
	local buf = ""
	
	while true do
		local line, err, partial = sock:receive()
		
		if line then
			return buf .. line
		elseif err == "timeout" then
			buf = buf .. partial
		else
			return nil
		end
		
		task.sleep_io(sock)
	end
end

function InteractConnection:read_data()
	local sock = self.sock
	local amt = tonumber(self:read_line())
	local buf = ""
	
	while buf:len() < amt do
		local data, err, partial = sock:receive(amt - buf:len())
		
		if data then
			buf = buf .. data
			break
		elseif err == "timeout" then
			buf = buf .. partial
		else
			return nil
		end
		
		task.sleep_io(sock)
	end
	
	return buf
end
		
