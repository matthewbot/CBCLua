-- This tiny program is packaged with cbclua
-- and is automatically overwritten when new code 
-- is loaded from a thumb drive

local cbc = require "cbclua.cbc"
local task = require "cbclua.task"

function main()
	print("Push A button")
	task.wait(cbc.a_button)
	print("Hello World! From lua!")
end
