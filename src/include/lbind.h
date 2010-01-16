#ifndef CBCLUA_LBIND_H
#define CBCLUA_LBIND_H

#include <string>
#include <lua.hpp>

// binder definitions

template <void (* func)()> static int lbind(lua_State *L) {
	func();
	return 0;
}

template <int (* func)()> static int lbind(lua_State *L) {
	lua_pushinteger(L, func());
	return 1;
}

template <void (* func)(int)> static int lbind(lua_State *L) {
	func(luaL_checkint(L, 1));
	return 0;
}

template <int (* func)(int)> static int lbind(lua_State *L) {
	int ret = func(luaL_checkint(L, 1));
	lua_pushinteger(L, ret);
	return 1;
}

template <void (* func)(int, int)> static int lbind(lua_State *L) {
	func(luaL_checkint(L, 1), luaL_checkint(L, 2));
	return 0;
}

template <void (* func)(int, int, int, int, int, int, int)> static int lbind(lua_State *L) {
	func(luaL_checkint(L, 1), luaL_checkint(L, 2), luaL_checkint(L, 3), luaL_checkint(L, 4),
		luaL_checkint(L, 5), luaL_checkint(L, 6), luaL_checkint(L, 7));
	return 0;
}

template <int (* func)(int, int)> static int lbind(lua_State *L) {
	int ret = func(luaL_checkint(L, 1), luaL_checkint(L, 2));
	lua_pushinteger(L, ret);
	return 1;
}

template <float (* func)(int, int)> static int lbind(lua_State *L) {
	float ret = func(luaL_checkint(L, 1), luaL_checkint(L, 2));
	lua_pushnumber(L, ret);
	return 1;
}

template <int (* func)(int, int, int)> static int lbind(lua_State *L) {
	int ret = func(luaL_checkint(L, 1), luaL_checkint(L, 2), luaL_checkint(L, 3));
	lua_pushinteger(L, ret);
	return 1;
}

template <int (* func)()> static int lbind_bool(lua_State *L) {
	lua_pushboolean(L, func() != 0);
	return 1;
}

template <int (* func)(int)> static int lbind_bool(lua_State *L) {
	int ret = func(luaL_checkint(L, 1));
	lua_pushboolean(L, ret != 0);
	return 1;
}

template <bool (* func)(double)> static int lbind(lua_State *L) {
	lua_pushboolean(L, func(luaL_checknumber(L, 1)));
	return 1;
}

template <double (* func)()> static int lbind(lua_State *L) {
	lua_pushnumber(L, func());
	return 1;
}

template <unsigned long (* func)()> static int lbind(lua_State *L) {
	lua_pushinteger(L, func());
	return 1;
}

template <void (* func)(double)> static int lbind(lua_State *L) {
	func(luaL_checknumber(L, 1));
	return 0;
}

#endif
