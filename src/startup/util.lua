-- some misc utilities for starting up

local traceback = debug.traceback
function cbctraceback(...)
	local out = traceback(...)
	return (out:gsub("\t", "  "))
end

local startup
function registerstartup(func)
	if startup then error("Cannot register two startup functions!") end
	startup = func
end

function dostartup()
	if startup then startup() end
end
	
function hasarg(val)
	for _,v in pairs(arg) do
		if v == val then
			return true
		end
	end
	
	return false
end
