module(...)

local task = require "std.task.task"

submodule("std.task.control")
submodule("std.task.list")
submodule("std.task.signal")
get_current = task.get_current -- export one function from task

