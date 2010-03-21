module("sched.mod")
export "cbclua.sched.taskentry"

export_partial("cbclua.sched.activelist", "add_task", "remove_task", "get_count", "get_current")
export_partial("cbclua.sched.timerlist", "TimerEntry", "register_timer", "unregister_timer", "new_timer")
export_partial("cbclua.sched.iolist", "IOEntry", "register_ioentry", "unregister_ioentry", "new_ioentry")

