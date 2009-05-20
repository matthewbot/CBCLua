module("main")

-- This tiny program is packaged with cbclua
-- and is automatically overwritten when new code 
-- is loaded from a thumb drive

local cbc = require "std.cbc"
local task = require "std.task"

function main()
	print("Push black button")
	task.wait(cbc.black_button)
	print("Hello World! From lua!")
end
