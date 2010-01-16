#include "bindings.h"
#include <stdio.h>
#include <lua.hpp>
#include <unistd.h>
#include <sys/stat.h>

static int lua_read(lua_State *L);
static int lua_eof(lua_State *L);
static int lua_mkdir(lua_State *L);

const luaL_Reg luafuncs[] = {
	{"read", lua_read},
	{"eof", lua_eof},
	{"mkdir", lua_mkdir},
	
	{NULL, NULL}
};

static int lua_read(lua_State *L) {
	FILE **file = (FILE **)lua_touserdata(L, 1);
	if (file == NULL || *file == NULL) 
		return 0;
	
	fflush(*file);
	int fd = fileno(*file);
	
	static char buf[100];
	int got = read(fd, buf, sizeof(buf));
	
	lua_pushlstring(L, buf, got);
	return 1;
}

static int lua_eof(lua_State *L) {
	FILE **file = (FILE **)lua_touserdata(L, 1);
	if (file == NULL || *file == NULL)
		return 0;
		
	lua_pushboolean(L, feof(*file) != 0);
	
	return 1;
}

static int lua_mkdir(lua_State *L) {
	const char *dirname = lua_tostring(L, 1);
	int result = mkdir(dirname, S_IRWXU | S_IRWXG | S_IRWXO);
	lua_pushboolean(L, result == 0);
	return 1;
}

