module("main")

local cbc = require "std.cbc"
local task = require "std.task"

function blah()
	while true do
	    task.sleep(.5)
	    print("foo?")
	    task.sleep(1.2)
	    print("bar!")
	end
end

function main() 
    task.start(blah)
    while true do
        task.sleep(1)
        print("hi")
    end
end



