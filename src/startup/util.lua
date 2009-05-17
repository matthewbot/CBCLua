-- some misc utilities for starting up

local traceback = debug.traceback
function cbctraceback(...)
	local out = traceback(...)
	return (out:gsub("\t", "  "))
end

function io.writeln(...)
	return io.write(..., "\n")
end
