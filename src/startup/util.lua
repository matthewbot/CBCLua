-- some handy global functions

function table.findvalue(table, val)
	for k,v in pairs(table) do
		if v == val then
			return k
		end
	end
end

local traceback = debug.traceback
function cbctraceback(...)
	local out = traceback(...)
	return (out:gsub("\t", "  "))
end
	
