#include "bindings.h"
#include "timer.h"
#include "watchdog.h"
#include <lua.hpp>
#include <lbind.h>
#include <vector>

using namespace std;

static int lua_sleep_select(lua_State *L);

const luaL_Reg luafuncs[] = {
	{"mseconds", lbind<mseconds>},
	{"seconds", lbind<seconds>},
	{"sleep_select", lua_sleep_select},
	{"watchdog", lbind<watchdog>},
	{"watchdog_yield", lbind<watchdog_yield>},
	
	{NULL, NULL}
};

static int get_fd(lua_State *L, int i);

static int lua_sleep_select(lua_State *L) {
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
	
	vector<bool> results = sleep_select(timeout, fds);
	
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
