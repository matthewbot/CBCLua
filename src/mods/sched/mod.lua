module("sched.mod")

export "cbclua.sched.entry"

local sched = require "cbclua.sched.sched"
get_current_task = sched.get_current_task

