module(...)

-- This tiny program is packaged with cbclua
-- and is automatically overwritten when new code 
-- is loaded from a thumb drive

local cbc = require "cbclua.cbc"
local task = require "cbclua.task"

function main()
	print("Push black button")
	task.wait(cbc.black_button)
	print("Hello World! From lua!")
end
