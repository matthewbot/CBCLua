#include "bindings.h"
#include "usercode.hpp"
#include <lua.hpp>
#include <cstdlib>

using namespace std;

extern "C" int luaopen_raw_cbc(lua_State *L) {
	lua_newtable(L);
	luaL_register(L, NULL, luafuncs);
	
	return 1;
}
