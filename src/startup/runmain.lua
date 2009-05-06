-- Load the main module

local status, result = xpcall(function() 
	return require("main") 
end, function (errmsg)
	print(debug.traceback("error while loading main module: " .. errmsg, 2))
	os.exit(1)
end)

-- Make sure the main module looks ok

local mainmod = result

if type(mainmod) ~= "table" then
	error("main module is missing the module header")
end
	
local mainfunc = mainmod.main
	
if mainfunc == nil then
	error("main module is missing main function")
end

local task = require "std.task"
local log = require "std.log"

log.start() -- start logging
task.start(mainfunc, "main")
print("Program started")
