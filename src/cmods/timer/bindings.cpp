#include "bindings.h"
#include "timer.h"
#include <lua.hpp>

static int lua_mseconds(lua_State *L) {
	lua_pushinteger(L, mseconds());
	return 1;
}

static int lua_seconds(lua_State *L) {
	lua_pushnumber(L, seconds());
	return 1;
}

static int lua_reset(lua_State *L) {
	reset();
	return 0;
}

const luaL_Reg luafuncs[] = {
	{"mseconds", lua_mseconds},
	{"seconds", lua_seconds},
	{"reset", lua_reset},
	
	{NULL, NULL}
};
