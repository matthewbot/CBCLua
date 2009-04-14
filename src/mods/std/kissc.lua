module("std.kissc")

function compat(depth)
	import("raw.cbc", depth+1);
	import("std.kissc", depth+1);
end

local task = require "std.task"

sleep = task.sleep
