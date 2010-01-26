local task = require "cbclua.task"
local evalenv = require "cbclua.interact.evalenv"
local rawio = require "cbclua.rawio"
local cbc = require "cbclua.cbc"
local maintask = require "cbclua.maintask"
local os = require "os"
local io = require "io"
import "cbclua.interact.config"

local globalenv = evalenv.EvalEnvironment()

InteractConnection = create_class "InteractConnection"

function InteractConnection:construct(sock, callback)
	self.sock = sock
	self.callback = callback
	self.task = task.start(function () return self:run() end, "interact conn", false, true)
end

function InteractConnection:run()
	self.sock:settimeout(0)
	self:write_line(cbclua_get_name())
	self:write_line(cbclua_get_version())
	
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
		self:write_data(tostring(result))
	end, "interact eval", true, true)
end

function InteractConnection:cmd_runmain()
	if maintask.is_running() then
		self:write_line("ERROR")
		self:write_data("Program already running")
	else
		local ok, err = maintask.run()
	
		if not ok then
			self:write_line("ERROR")
			self:write_data("Error while running main:\n" .. err)
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
	task.stop_all_user_tasks()
end

function InteractConnection:cmd_clearcode()
	os.execute("rm -rf " .. cbclua_get_codepath() .. "/*")
	cbclua_unload_all_codemods()
	globalenv = evalenv.EvalEnvironment() -- start new execution environment
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

function InteractConnection:send_print(data)
	self:write_line("PRINT")
	self:write_data(data)
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
		
