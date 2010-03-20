module("cbclua.sched.timerlist")
local timer = require "cbclua.timer"
local table = require "table"

local timerlist = { }

function register_timer(timer)
	local ourtime = timer.time
	
	local pos = 1
	while pos <= #timerlist and timerlist[pos].time < ourtime do
		pos = pos + 1
	end
	
	table.insert(timerlist, pos, timer)
end

function cancel_timer(timer)
	local pos = table.findvalue(timerlist, timer)
	table.remove(timerlist, pos)
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

