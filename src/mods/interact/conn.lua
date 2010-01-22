local task = require "cbclua.task"
local evalenv = require "cbclua.interact.evalenv"
local rawio = require "cbclua.rawio"
local cbc = require "cbclua.cbc"
local os = require "os"
local io = require "io"
import "cbclua.interact.config"

local maintask
local globalenv = evalenv.EvalEnvironment()

InteractConnection = create_class "InteractConnection"

function InteractConnection:construct(sock, callback)
	self.sock = sock
	self.callback = callback
	self.task = task.start(function () return self:run() end, "interact conn", true, false, true)
end

local CODEPATH = _G.CBCLUA_CODEPATH

function InteractConnection:run()
	self.sock:settimeout(0)
	self:writeLn(_G.CBCLUA_NAME)
	self:writeLn(_G.CBCLUA_VERSION)
	
	while true do
		local command = self:readLn()
		
		if command == nil then
			break
		elseif command == "EXPR" then
			local expr = self:readData()
			task.start(function ()
				local ok, result = globalenv:run(expr)
				if ok then
					self:writeLn("RESULT")
				else
					self:writeLn("ERROR")
				end
				self:writeData(tostring(result))
			end, "interact eval", true, true)
		elseif command == "RUNMAIN" then
			if maintask and maintask:running() then
				self:writeLn("ERROR")
				self:writeData("Program already running")
			else
				local ok, err = pcall(function ()
					local mainmod = require "main"
					maintask = task.start(mainmod.main, "Main task")
				end)
			
				if not ok then
					self:writeLn("ERROR")
					self:writeData("Error while running main:\n" .. err)
				end
			end
		elseif command == "BUTTONDOWN" then
			local buttonname = self:readLn()
			cbc.buttons[buttonname]:press()
		elseif command == "BUTTONUP" then
			local buttonname = self:readLn()
			cbc.buttons[buttonname]:release()
		elseif command == "STOPTASKS" then
			task.stop_all_user_tasks()
			maintask = nil
		elseif command == "CLEARCODE" then
			os.execute("rm -rf " .. _G.CBCLUA_CODEPATH .. "/*")
			unload_all_codemods()
			globalenv = evalenv.EvalEnvironment() -- start new execution environment
		elseif command == "MKCODEDIR" then
			local dir = self:readLn()
			rawio.mkdir(_G.CBCLUA_CODEPATH .. "/" .. dir)
		elseif command == "PUTCODE" then
			local filename = self:readLn()
			local filedata = self:readData()
			
			local file = io.open(_G.CBCLUA_CODEPATH .. "/" .. filename, "w")
			if file then
				file:write(filedata)
				file:close()
			end
		end
	end
	
	self.sock:close()
	self.callback.on_conn_close(self)
end

function InteractConnection:send_print(data)
	self:writeLn("PRINT")
	self:writeData(data)
end

function InteractConnection:writeLn(line)
	self:sendAll(line .. "\n")
end

function InteractConnection:writeData(data)
	self:writeLn(data:len())
	self:sendAll(data)
end
	
function InteractConnection:sendAll(data)
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
	
function InteractConnection:readLn()
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

function InteractConnection:readData()
	local sock = self.sock
	local amt = tonumber(self:readLn())
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
		
