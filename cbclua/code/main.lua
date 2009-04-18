module("main")

-- This tiny program is packaged with cbclua
-- and is automatically overwritten when new code 
-- is loaded from a thumb drive

local cbc = require "std.cbc"
local util = require "std.util"

function main()
	print("Push black button")
	util.wait(cbc.black_button)
	print("Hello World! From lua!")
end
