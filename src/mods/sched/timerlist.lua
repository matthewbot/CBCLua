module("cbclua.sched.timerlist")
local timer = require "cbclua.timer"
local table = require "table"

--[[ TimerEntry ]]--

TimerEntry = create_class "TimerEntry"

function TimerEntry:construct(time, func)
	self.time = time
	self.func = func
end

function TimerEntry:get_time()
	return self.time
end

function TimerEntry:get_time_remaining(curtime)
	curtime = curtime or timer.seconds()
	return self.time - curtime
end

function TimerEntry:run()
	return self.func()
end

--[[ timerlist ]]--

local timerlist = { }

function register_timer(timer)
	local ourtime = timer.time
	
	local pos = 1
	while pos <= #timerlist and timerlist[pos].time < ourtime do
		pos = pos + 1
	end
	
	table.insert(timerlist, pos, timer)
end

function unregister_timer(timer)
	local pos = table.findvalue(timerlist, timer)
	if not pos then return end
	
	table.remove(timerlist, pos)
end

function new_timer(...)
	local timer = TimerEntry(...)
	register_timer(timer)
	return timer
end

--[[ main funcs ]]--

function get_count()
	return #timerlist
end

function get_next_time()
	if #timerlist >= 1 then
		return timerlist[1]:get_time()
	end
end

function run()
	local curtime = timer.seconds()
	
	while #timerlist >= 1 do
		local timer = timerlist[1]
		if timer:get_time_remaining(curtime) > 0 then
			break
		end
		
		timer:run()
		table.remove(timerlist, 1)
	end
end

