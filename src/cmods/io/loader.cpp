#include "bindings.h"
#include <lua.hpp>

using namespace std;

extern "C" int luaopen_cbclua_rawio(lua_State *L) {
	lua_newtable(L);
	luaL_register(L, NULL, luafuncs);
	
	return 1;
}
