-- some misc utilities for starting up

local traceback = debug.traceback
function cbctraceback(...)
	local out = traceback(...)
	return (out:gsub("\t", "  "))
end

function io.writeln(...)
	return io.write(..., "\n")
end

function debug.findlocal(level, name)
	level = level+1
	local num=1
	while true do
		local localname, localval = debug.getlocal(level, num)
		if localname == name then
			return localval
		elseif localname == nil then
			return nil
		end
		num = num+1
	end
end
