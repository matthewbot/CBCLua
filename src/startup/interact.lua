local interact = require "std.interact"
local task = require "std.task"
		
if coroutine.coco == nil then
print[[
No CoCo in this lua interpreter.
This means you cannot call any
blocking task functions from within
an interaction command.
----------]]
end
		
interact.start()
print("Interact started")
