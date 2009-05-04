#include "bindings.h"
#include "timer.h"
#include "watchdog.h"
#include <lua.hpp>
#include <lbind.h>
#include <vector>
#include <cstdio>
#include <unistd.h>

using namespace std;

static int lua_raw_sleep(lua_State *L);
static int lua_getio(lua_State *L);

const luaL_Reg luafuncs[] = {
	{"raw_seconds", lbind<raw_seconds>},
	{"raw_mseconds", lbind<raw_mseconds>},
	{"mseconds", lbind<mseconds>},
	{"seconds", lbind<seconds>},
	{"raw_sleep", lua_raw_sleep},
	{"raw_yield", lbind<raw_yield>},
	{"watchdog", lbind<watchdog>},
	{"watchdog_term", lbind<watchdog_term>},
	{"raw_getio", lua_getio},
	
	{NULL, NULL}
};

static int lua_raw_sleep(lua_State *L) {
	double timeout = checknumber(L, 1);
	
	int fdcount = lua_gettop(L)-1;
	vector<int> fds;
	fds.reserve(fdcount);
	
	for (int i=0; i < fdcount; i++) {
		FILE **file = (FILE **)lua_touserdata(L, i+2);
		fds.push_back(fileno(*file));
	}
	
	vector<bool> results = raw_sleep(timeout, fds);
	
	for (vector<bool>::iterator i = results.begin(); i != results.end(); ++i) {
		lua_pushboolean(L, *i);
	}
	
	return fdcount;
}

// -- NOTE: If this sprouts any more IO functions, make a new module

static int lua_getio(lua_State *L) {
	FILE **file = (FILE **)lua_touserdata(L, 1);
	fflush(*file);
	int fd = fileno(*file);
	
	static char buf[100];
	int got = read(fd, buf, sizeof(buf));
	
	lua_pushlstring(L, buf, got);
	return 1;
}
