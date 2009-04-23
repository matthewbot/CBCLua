#include "bindings.h"
#include "usercode.hpp"
#include <lbind.h>

using namespace std;

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
	{"digital", lbind_bool<digital>},
	
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
	
	{"black_button", lbind_bool<black_button>},
	{"up_button", lbind_bool<up_button>},
	{"down_button", lbind_bool<down_button>},
	{"left_button", lbind_bool<left_button>},
	{"right_button", lbind_bool<right_button>},
	{"a_button", lbind_bool<a_button>},
	{"b_button", lbind_bool<b_button>},
	
	{"__errwrap", errwrap},
	
	{NULL, NULL}
};



