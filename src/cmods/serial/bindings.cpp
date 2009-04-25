#include "bindings.h"
#include "serial.h"
#include <lua.hpp>
#include <lbind.h>

static int lread(lua_State *L);
static int lwrite(lua_State *L);

const luaL_Reg luafuncs[] = { 
	{"init", lbind<serial::init>},
	{"quit", lbind<serial::quit>},
	
	{"poll", lbind<serial::poll>},
	{"read", lread},
	{"write", lwrite},
	
	{NULL, NULL}
};

static int lread(lua_State *L) {
	static char buf[2048];
	
	int got = serial::read(buf, sizeof(buf));
	lua_pushlstring(L, buf, got);
	
	return 1;
}

static int lwrite(lua_State *L) {
	size_t length;
	const char *buf = checklstring(L, 1, &length);
	
	serial::write(buf, length);
	
	return 0;
}
