-- Load the main module

local status, result = xpcall(function() 
	return require("main") 
end, function (errmsg)
	cbclua_log(debug.traceback("error while loading main module: " .. errmsg, 2))
	os.exit(1)
end)

-- Make sure the main module looks ok

local mainmod = result

if type(mainmod) ~= "table" then
	cbclua_log("main module is missing the module header")
	os.exit(1)
end
	
local mainfunc = mainmod.main
	
if mainfunc == nil then
	cbclua_log("main module is missing main function")
	os.exit(1)
end

local task = require "std.task"
task.start(mainfunc, "main")

-- Print a final status message
dostartup() -- if the program has a startup function run it

