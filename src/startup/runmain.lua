-- Load the main module

local status, result = xpcall(function() 
	return require("main") 
end, function (errmsg)
	print(debug.traceback("cbclua: error while loading main module: " .. errmsg, 2))
end)

if status == false then
	os.exit(1)
end

-- Make sure the main module looks ok

local mainmod = result

if type(mainmod) ~= "table" then
	print("cbclua: main module is missing the module header")
	os.exit(1)
end
	
local mainfunc = mainmod.main
	
if mainfunc == nil then
	print("cbclua: main module is missing main function")
	os.exit(1)
end

local task = require "std.task"
task.start(mainfunc, "main")

-- Print a final status message
print("cbclua: starting main task")
dostartup() -- if the program has a startup function run it

