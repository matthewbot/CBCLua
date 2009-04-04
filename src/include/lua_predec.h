#ifndef CBCLUA_LUA_PREDEC_HPP
#define CBCLUA_LUA_PREDEC_HPP

extern "C" {
	typedef struct lua_State lua_State;
	typedef int (*lua_CFunction) (lua_State *L);
	typedef struct luaL_Reg luaL_Reg;
}

#endif
