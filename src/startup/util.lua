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

-- Small error handling improvement

local realerror = error
local errorlvl = 0

-- runs func, but causes every error line number to refer to one caller before the current
-- IE, any errors occuring in the block blame the current function's caller, not the current function
function pass_errors(func, ...) 
	errorlvl = errorlvl + 2
	local results = { func(...) }
	errorlvl = errorlvl - 2
	return unpack(results)
end

function error(msg, depth)
	depth = depth or 1
	depth = depth + errorlvl
	return realerror(msg, depth+1)
end

function assert(val, msg)
	if not val then
		error(msg or "assertion failed!", 2)
	end
	
	return val
end

-- shutdown hooks

local shutdown_hooks = { }

function add_shutdown_hook(hook)
	table.insert(shutdown_hooks, hook)
end

function run_shutdown_hooks()
	for i=#shutdown_hooks,1,-1 do
		local hook = shutdown_hooks[i]
		pcall(hook)
	end
end
		
