local task = require "cbclua.task"
local tasks = require "cbclua.interact.tasks"
require "cbclua.interact.hooks" -- set up interaction hooks

function start_tasks()
	task.start(tasks.listen, "interact listener", false, false, true)
	task.start(tasks.respond, "interact respond", true, false, true)
end

