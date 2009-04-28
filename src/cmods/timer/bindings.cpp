#include "bindings.h"
#include "timer.h"
#include "watchdog.h"
#include <lua.hpp>
#include <lbind.h>

const luaL_Reg luafuncs[] = {
	{"raw_seconds", lbind<raw_seconds>},
	{"raw_mseconds", lbind<raw_mseconds>},
	{"mseconds", lbind<mseconds>},
	{"seconds", lbind<seconds>},
	{"rawsleep", lbind<rawsleep>},
	{"rawyield", lbind<rawyield>},
	{"watchdog", lbind<watchdog>},
	{"watchdog_disable", lbind<watchdog_disable>},
	{"watchdog_term", lbind<watchdog_term>},
	
	{NULL, NULL}
};
