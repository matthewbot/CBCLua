module("cbclua.sched.iolist")
local timer = require "cbclua.timer"
local table = require "table"
local math = require "math"

--[[ IOEntry ]]--

IOEntry = create_class "IOEntry"

function IOEntry:construct(file, func)
	self.file = file
	self.func = func
end

function IOEntry:get_file()
	return self.file
end

function IOEntry:run()
	return self.func()
end

--[[ iolist ]]--

local iolist = { }

function register_ioentry(ioentry)
	table.insert(iolist, ioentry)
end

function unregister_ioentry(ioentry)
	local pos = table.findvalue(iolist, ioentry)
	if not pos then return end
	table.remove(iolist, pos)
end

function new_ioentry(...)
	local ioentry = IOEntry(...)
	register_ioentry(ioentry)
	return ioentry
end

--[[ main funcs ]]--

function block(sleepamt)
	if sleepamt == math.inf then
		sleepamt = -1
	end

	local files = table.map(function (ioentry) return ioentry:get_file() end, iolist)
	local flags = { timer.sleep_select(sleepamt, unpack(files)) }
	
	local runentries = { }
	for i, entry in ipairs(iolist) do
		if flags[i] then
			table.insert(runentries, entry)
		end
	end
	
	for entry in table.values(runentries) do
		unregister_ioentry(entry)
		entry:run()
	end
end

