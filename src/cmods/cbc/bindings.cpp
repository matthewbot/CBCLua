#include "bindings.h"
#include "usercode.hpp"
#include <lua.hpp>
#include <sstream>

using namespace std;

// generic binders
template <void (* func)()>             static int lbind(lua_State *L);
template <int (* func)()>              static int lbind(lua_State *L);
template <void (* func)(int)>          static int lbind(lua_State *L);
template <int (* func)(int)>           static int lbind(lua_State *L);
template <void (* func)(int, int)>     static int lbind(lua_State *L);
template <int (* func)(int, int)>      static int lbind(lua_State *L);
template <float (* func)(int, int)>    static int lbind(lua_State *L);
template <int (* func)(int, int, int)> static int lbind(lua_State *L);


// function table

static int errwrap(lua_State *L);

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
	
	{"track_is_new_data_available", lbind<track_is_new_data_available>},
	{"track_update", lbind<track_update>},
	{"track_get_frame", lbind<track_get_frame>},
	{"track_count", lbind<track_count>},
	{"track_size", lbind<track_size>},
	{"track_x", lbind<track_x>},
	{"track_y", lbind<track_y>},
	{"track_confidence", lbind<track_confidence>},
	{"track_bbox_left", lbind<track_bbox_left>},
	{"track_bbox_right", lbind<track_bbox_right>},
	{"track_bbox_top", lbind<track_bbox_top>},
	{"track_bbox_bottom", lbind<track_bbox_bottom>},
	{"track_bbox_width", lbind<track_bbox_width>},
	{"track_bbox_height", lbind<track_bbox_height>},
	{"track_angle", lbind<track_angle>},
	{"track_major_axis", lbind<track_major_axis>},
	{"track_minor_axis", lbind<track_minor_axis>},
	{"track_capture_time", lbind<track_capture_time>},
	{"track_previous_capture_time", lbind<track_previous_capture_time>},
	
	{"black_button", lbind<black_button>},
	{"up_button", lbind<up_button>},
	{"down_button", lbind<down_button>},
	{"left_button", lbind<left_button>},
	{"right_button", lbind<right_button>},
	{"a_button", lbind<a_button>},
	{"b_button", lbind<b_button>},
	
	{"__errwrap", errwrap},
	
	{NULL, NULL}
};

// error wrapper (used inside OO library to rewrite errors and make them appear OO-like)

static bool errwrapped; // whether the errors in the function should be wrapped
static int errlevel; // the depth to report errors at
static string errfuncname; // the name to use as the function

static int errwrap(lua_State *L) {
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

static int checkint(lua_State *L, int pos) {
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
		return luaL_typerror(L, pos, tname);
	}
}

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

