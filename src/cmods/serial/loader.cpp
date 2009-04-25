#include "bindings.h"
#include <lua.hpp>

extern "C" int luaopen_raw_serial(lua_State *L) {
	lua_newtable(L);
	luaL_register(L, NULL, luafuncs);
	
	return 1;
}
