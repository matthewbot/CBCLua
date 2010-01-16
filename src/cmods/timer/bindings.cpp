#include "bindings.h"
#include "timer.h"
#include "watchdog.h"
#include <lua.hpp>
#include <lbind.h>
#include <vector>

using namespace std;

static int lua_raw_sleep(lua_State *L);

const luaL_Reg luafuncs[] = {
	{"raw_seconds", lbind<raw_seconds>},
	{"raw_mseconds", lbind<raw_mseconds>},
	{"mseconds", lbind<mseconds>},
	{"seconds", lbind<seconds>},
	{"raw_sleep", lua_raw_sleep},
	{"raw_yield", lbind<raw_yield>},
	{"watchdog", lbind<watchdog>},
	{"watchdog_term", lbind<watchdog_term>},
	
	{NULL, NULL}
};

static int get_fd(lua_State *L, int i);

static int lua_raw_sleep(lua_State *L) {
	double timeout = luaL_checknumber(L, 1);
	
	int fdcount = lua_gettop(L)-1;
	vector<int> fds;
	fds.reserve(fdcount);
	
	for (int i=0; i < fdcount; i++) {
		int fd = get_fd(L, i+2);
	
		if (fd == -1) {
			FILE **file = (FILE **)lua_touserdata(L, i+2);
			if (file != NULL && *file != NULL)
				fd = fileno(*file);
		}
		
		if (fd != -1)
			fds.push_back(fd);
	}
	
	vector<bool> results = raw_sleep(timeout, fds);
	
	for (vector<bool>::iterator i = results.begin(); i != results.end(); ++i) {
		lua_pushboolean(L, *i);
	}
	
	return fdcount;
}

static int get_fd(lua_State *L, int i) {
	int fd=-1;
    lua_pushstring(L, "getfd");
    lua_gettable(L, i);
    if (!lua_isnil(L, -1)) {
        lua_pushvalue(L, i);
        lua_call(L, 1, 1);
        if (lua_isnumber(L, -1)) 
            fd = lua_tonumber(L, -1); 
    } 
    lua_pop(L, 1);
    return fd;
}
