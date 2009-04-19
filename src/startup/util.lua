-- some handy global functions

function table.findvalue(table, val)
	for k,v in pairs(table) do
		if v == val then
			return k
		end
	end
end

function table.deepclone(table)
	local newtable = { }
	for k,v in pairs(table) do
		if type(v) == "table" then
			newtable[k] = table.deepclone(v)
		else
			newtable[k] = v
		end
	end
	
	return newtable
end

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
	
