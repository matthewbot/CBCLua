#include "bindings.h"
#include "timer.h"
#include "watchdog.h"
#include <lua.hpp>

static int lua_mseconds(lua_State *L) {
	lua_pushinteger(L, mseconds());
	return 1;
}

static int lua_raw_mseconds(lua_State *L) {
	lua_pushinteger(L, raw_mseconds());
	return 1;
}
	
static int lua_seconds(lua_State *L) {
	lua_pushnumber(L, seconds());
	return 1;
}

static int lua_raw_seconds(lua_State *L) {
	lua_pushnumber(L, raw_seconds());
	return 1;
}

static int lua_reset(lua_State *L) {
	reset();
	return 0;
}

static int lua_watchdog(lua_State *L) {
	watchdog();
	return 0;
}

const luaL_Reg luafuncs[] = {
	{"raw_seconds", lua_raw_seconds},
	{"raw_mseconds", lua_raw_mseconds},
	{"mseconds", lua_mseconds},
	{"seconds", lua_seconds},
	{"reset", lua_reset},
	{"watchdog", lua_watchdog},
	
	{NULL, NULL}
};
