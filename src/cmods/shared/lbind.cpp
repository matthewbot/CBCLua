#include <lua.hpp>
#include <sstream>
#include <lua.hpp>
#include "lbind.h"

static bool errwrapped; // whether the errors in the function should be wrapped
static int errlevel; // the depth to report errors at
static std::string errfuncname; // the name to use as the function

int errwrap(lua_State *L) {
	errlevel = lua_tointeger(L, 1);
	lua_remove(L, 1);
	
	errfuncname = lua_tostring(L, 1);
	lua_remove(L, 1);
	
	lua_CFunction func = lua_tocfunction(L, 1);
	lua_remove(L, 1);
	
	errwrapped = true;
	int results = func(L);
	errwrapped = false;
	return results;
}

int checkint(lua_State *L, int pos) {
	int type = lua_type(L, pos);
	if (type == LUA_TNUMBER)
		return lua_tointeger(L, pos);

	const char *tname = lua_typename(L, type);

	if (errwrapped) {
		luaL_where(L, errlevel);
		lua_pushfstring(L, "bad argument %d to '%s' (integer expected, got %s)", pos, errfuncname.c_str(), tname);
		lua_concat(L, 2);
		return lua_error(L);
	} else {
		return luaL_typerror(L, pos, "int");
	}
}

double checknumber(lua_State *L, int pos) {
	int type = lua_type(L, pos);
	if (type == LUA_TNUMBER)
		return lua_tonumber(L, pos);

	const char *tname = lua_typename(L, type);

	if (errwrapped) {
		luaL_where(L, errlevel);
		lua_pushfstring(L, "bad argument %d to '%s' (number expected, got %s)", pos, errfuncname.c_str(), tname);
		lua_concat(L, 2);
		return lua_error(L);
	} else {
		return luaL_typerror(L, pos, "number");
	}
}

const char *checklstring(lua_State *L, int pos, size_t *len) {
	int type = lua_type(L, pos);
	if (type == LUA_TSTRING)
		return lua_tolstring(L, pos, len);

	const char *tname = lua_typename(L, type);

	if (errwrapped) {
		luaL_where(L, errlevel);
		lua_pushfstring(L, "bad argument %d to '%s' (string expected, got %s)", pos, errfuncname.c_str(), tname);
		lua_concat(L, 2);
		lua_error(L);
	} else {
		luaL_typerror(L, pos, "string");
	}
	
	return NULL; // shut compiler up, code never gets here
}

