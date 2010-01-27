#include "bindings.h"
#include "usercode.hpp"
#include <lua.hpp>
#include <cstdlib>

using namespace std;

extern "C" int luaopen_cbclua_rawcbc(lua_State *L) {
	libcbc_init();

	lua_newtable(L);
	luaL_register(L, NULL, luafuncs);
	
	return 1;
}
