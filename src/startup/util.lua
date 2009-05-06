-- some misc utilities for starting up

local traceback = debug.traceback
function cbctraceback(...)
	local out = traceback(...)
	return (out:gsub("\t", "  "))
end
	
function hasarg(val)
	for _,v in pairs(arg) do
		if v == val then
			return true
		end
	end
	
	return false
end

function io.writeln(...)
	return io.write(..., "\n")
end
