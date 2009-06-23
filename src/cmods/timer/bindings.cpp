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

static int lua_raw_sleep(lua_State *L) {
	double timeout = checknumber(L, 1);
	
	int fdcount = lua_gettop(L)-1;
	vector<int> fds;
	fds.reserve(fdcount);
	
	for (int i=0; i < fdcount; i++) {
		FILE **file = (FILE **)lua_touserdata(L, i+2);
		if (file != NULL && *file != NULL)
			fds.push_back(fileno(*file));
	}
	
	vector<bool> results = raw_sleep(timeout, fds);
	
	for (vector<bool>::iterator i = results.begin(); i != results.end(); ++i) {
		lua_pushboolean(L, *i);
	}
	
	return fdcount;
}
