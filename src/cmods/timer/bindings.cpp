#include "bindings.h"
#include "timer.h"
#include "watchdog.h"
#include <sched.h>
#include <lua.hpp>
#include <lbind.h>

const luaL_Reg luafuncs[] = {
	{"raw_seconds", lbind<raw_seconds>},
	{"raw_mseconds", lbind<raw_mseconds>},
	{"mseconds", lbind<mseconds>},
	{"seconds", lbind<seconds>},
	{"reset", lbind<reset>},
	{"rawsleep", lbind<rawsleep>},
	{"watchdog", lbind<watchdog>},
	{"watchdog_disable", lbind<watchdog_disable>},
	{"watchdog_term", lbind<watchdog_term>},
	{"yield", lbind<sched_yield>},
	
	{NULL, NULL}
};
