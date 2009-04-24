#ifndef CBCLUA_LBIND_H
#define CBCLUA_LBIND_H

#include <string>
#include <lua.hpp>

// special error handling functions

int errwrap(lua_State *L); // calls a C function but can rewrite its error handling. Used in OO wrappers to make error messages nicer
int checkint(lua_State *L, int pos); // called by templates. Uses error information from above
double checknumber(lua_State *L, int pos);
const char *checklstring(lua_State *L, int pos, size_t *len);

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
	func(checkint(L, 1));
	return 0;
}

template <int (* func)(int)> static int lbind(lua_State *L) {
	int ret = func(checkint(L, 1));
	lua_pushinteger(L, ret);
	return 1;
}

template <void (* func)(int, int)> static int lbind(lua_State *L) {
	func(checkint(L, 1), checkint(L, 2));
	return 0;
}

template <int (* func)(int, int)> static int lbind(lua_State *L) {
	int ret = func(checkint(L, 1), checkint(L, 2));
	lua_pushinteger(L, ret);
	return 1;
}

template <float (* func)(int, int)> static int lbind(lua_State *L) {
	float ret = func(checkint(L, 1), checkint(L, 2));
	lua_pushnumber(L, ret);
	return 1;
}

template <int (* func)(int, int, int)> static int lbind(lua_State *L) {
	int ret = func(checkint(L, 1), checkint(L, 2), checkint(L, 3));
	lua_pushinteger(L, ret);
	return 1;
}

template <int (* func)()> static int lbind_bool(lua_State *L) {
	lua_pushboolean(L, func() != 0);
	return 1;
}

template <int (* func)(int)> static int lbind_bool(lua_State *L) {
	int ret = func(checkint(L, 1));
	lua_pushboolean(L, ret != 0);
	return 1;
}

template <bool (* func)(double)> static int lbind(lua_State *L) {
	lua_pushboolean(L, checknumber(L, 1));
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
	func(checknumber(L, 1));
	return 0;
}

#endif
