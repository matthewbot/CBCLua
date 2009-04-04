#include "bindings.h"
#include "usercode.hpp"
#include <lua.hpp>
#include <cstdlib>

using namespace std;

static void shutdowncbc();

extern "C" int luaopen_raw_cbc(lua_State *L) {
	lua_newtable(L);
	luaL_register(L, NULL, luafuncs);
	
	atexit(shutdowncbc);
	
	return 1;
}

static void shutdowncbc() {
	ao();
	disable_servos();
}
