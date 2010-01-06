-- This implements signals, which can block one or more processes until 
-- the signal is notified by another process. It is also possible for a process
-- to block until any one of a set of signals becomes notified

-- A nice little design break-through, the core task system knows nothing about signals.
-- This entire module is built using the public interface to the scheduler and the normal
-- task control methods

module(...)

local sched = require "cbclua.task.sched"
local timer = require "cbclua.timer"

import "cbclua.task.control"

--

Signal = create_class "Signal"

function Signal:construct()
	self.ctr = 0
end

function Signal:notify()
	self.ctr = self.ctr + 1
	sched.wake_all() -- on the next cycle, every task must get run (important to ensure that everyone checks their signals again)
end

function Signal:wait(time)
	local startctr = self.ctr

	if time then
		local endtime = timer.seconds() + time
		
		while true do
			local remaining = endtime - timer.seconds()
			
			if remaining <= 0 then return false end
			if self.ctr > startctr then return true end		
			
			sleep_cycle_till(remaining)
		end
	else
		repeat
			sleep_cycle()
		until self.ctr > startctr
		
		return true
	end
end

function sleep_signals(...)
	local signals = {...}
	
	local startctrs = { }
	for i=1,#signals do
		startctrs[i] = signals[i].ctr
	end

	while true do
		sleep_cycle()
		
		for i=1,#signals do
			local signal = signals[i]
			if signal.ctr > startctrs[i] then
				return signal
			end
		end
	end
end

