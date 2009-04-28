#include "bindings.h"
#include "timer.h"
#include <lua.hpp>

extern "C" int luaopen_std_timer(lua_State *L) {
	lua_newtable(L);
	luaL_register(L, NULL, luafuncs);
	
	start_timers();
	
	return 1;
}
