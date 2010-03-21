local activelist = require "cbclua.sched.activelist"
local timerlist = require "cbclua.sched.timerlist"
local iolist = require "cbclua.sched.iolist"
local timer = require "cbclua.timer"
local math = require "math"

function sched_main()
	while true do
		activelist.run()
		timerlist.run()
		
		local sleepamt
		if activelist.get_count() > 0 then
			sleepamt = 0
		elseif timerlist.get_count() > 0 then
			sleepamt = timerlist.get_next_time() - timer.seconds()
			if sleepamt < 0 then
				sleepamt = 0
			end
		else
			sleepamt = math.inf
		end
		
		iolist.block(sleepamt)
	end
end

