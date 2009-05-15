module(...)

local task = require "std.task.task"

submodule("std.task.control")
submodule("std.task.list")
get_current = task.get_current -- export one function from task

