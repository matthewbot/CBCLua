local task = require "cbclua.task"
local evalenv = require "cbclua.interact.evalenv"
import "cbclua.interact.config"

InteractConnection = create_class "InteractConnection"

function InteractConnection:construct(sock)
	self.sock = sock
	self.env = evalenv.EvalEnvironment()
	self.task = task.start(function () return self:run() end, "interact conn", true)
end

function InteractConnection:run()
	self.sock:settimeout(0)
	self:writeLn(CBCLUA_NAME)
	self:writeLn(CBCLUA_INTERACT_VERSION)
	
	while true do
		local command = self:readLn()
		
		if command == nil then
			break
		elseif command == "EXPR" then
			local expr = self:readData()
			task.async(function ()
				local ok, result = self.env:run(expr)
				if ok then
					self:writeLn("RESULT")
				else
					self:writeLn("ERROR")
				end
				self:writeData(tostring(result))
			end)
		end	
	end
	
	self.sock:close()
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
		
