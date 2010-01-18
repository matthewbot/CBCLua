#include "bindings.h"
#include "timer.h"
#include "watchdog.h"
#include <lua.hpp>
#include <cstdlib>

extern "C" int luaopen_cbclua_timer(lua_State *L) {
	lua_newtable(L);
	luaL_register(L, NULL, luafuncs);
	
	return 1;
}

