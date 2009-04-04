#include "bindings.h"
#include "usercode.hpp"
#include <lua.hpp>

using namespace std;

// generic binders
template <void (* func)()>             static int lbind(lua_State *L);
template <int (* func)()>              static int lbind(lua_State *L);
template <void (* func)(int)>          static int lbind(lua_State *L);
template <int (* func)(int)>           static int lbind(lua_State *L);
template <void (* func)(int, int)>     static int lbind(lua_State *L);
template <int (* func)(int, int)>      static int lbind(lua_State *L);
template <int (* func)(int, int, int)> static int lbind(lua_State *L);

// function table

const luaL_Reg luafuncs[] = { 
	{"fd", lbind<fd>},
	{"bk", lbind<bk>},
	{"motor", lbind<motor>},
	{"setpwm", lbind<setpwm>},
	{"off", lbind<off>},
	
	{"mav", lbind<mav>},
	{"mrp", lbind<mrp>},
	{"mtp", lbind<mtp>},
	{"bmd", lbind<bmd>},
	{"get_motor_done", lbind<get_motor_done>},
	{"clear_motor_position_counter", lbind<clear_motor_position_counter>},
	{"get_motor_position_counter", lbind<get_motor_position_counter>},
	
	{"accel_x", lbind<accel_x>},
	{"accel_y", lbind<accel_y>},
	{"accel_z", lbind<accel_z>},
	
	{"set_servo_position", lbind<set_servo_position>},
	{"enable_servos", lbind<enable_servos>},
	{"disable_servos", lbind<disable_servos>},
	
	{"analog10", lbind<analog10>},
	{"digital", lbind<digital>},
	
	{NULL, NULL}
};

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

template <int (* func)(int, int)> static int lbind(lua_State *L) {
	int ret = func(luaL_checkint(L, 1), luaL_checkint(L, 2));
	lua_pushinteger(L, ret);
	return 1;
}

template <int (* func)(int, int, int)> static int lbind(lua_State *L) {
	int ret = func(luaL_checkint(L, 1), luaL_checkint(L, 2), luaL_checkint(L, 3));
	lua_pushinteger(L, ret);
	return 1;
}

