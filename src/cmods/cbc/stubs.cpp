#ifndef CBCLUA_CBC

#include "usercode.hpp"

// TODO - Pull this out so it can be developed into a simulator

extern "C" {
	void tone(int frequency, int duration) { }
	void beep() { }
	int digital(int port) { return 0; } 
	int set_digital_output_value(int port, int value) { return 0; }
	int analog10(int port) { return 0; }
	int analog(int port) { return 0; }
	int accel_x() { return 0; } 
	int accel_y() { return 0; }
	int accel_z() { return 0; } 
	int sonar(int port) { return 0; }
	int sonar_inches(int port) { return 0; }
	float power_level() { return 0; }
	void enable_servos() { }
	void disable_servos() { }
	int set_servo_position(int servo, int pos) { return 0; }
	int get_servo_position(int servo) { return 0; }
	int get_motor_position_counter(int motor) { return 0; } 
	int clear_motor_position_counter(int motor) { return 0; } 
	int move_at_velocity(int motor, int velocity) { return 0; }
	int mav(int motor, int velocity) { return 0; }
	int move_to_position(int motor, int speed, int goal_pos) { return 0; }
	int mtp(int motor, int speed, int goal_pos) { return 0; }
	int move_relative_position(int motor, int speed, int delta_pos) { return 0; }
	int mrp(int motor, int speed, int delta_pos) { return 0; }
	void set_pid_gains(int motor, int p, int i, int d, int pd, int id, int dd) { }
	int freeze(int motor) { return 0; }
	int get_motor_done(int motor) { return 0; } 
	void block_motor_done(int motor) { }
	void bmd(int motor) { }
	int setpwm(int motor, int pwm) { return 0; }
	int getpwm(int motor) { return 0; }
	void fd(int motor) { }
	void bk(int motor) { }
	void motor (int motor, int percent) { }
	void off(int motor) { } 
	void ao() { } 

	void track_init() { }
	int track_is_new_data_available() { return 0; }
	void track_update() { }
	int track_get_frame() { return 0; }
	int track_count(int ch) { return 0; }
	int track_size(int ch, int i) { return 0; }
	int track_x(int ch, int i) { return 0; }
	int track_y(int ch, int i) { return 0; }
	int track_confidence(int ch, int i) { return 0; }
	int track_bbox_left(int ch, int i) { return 0; }
	int track_bbox_right(int ch, int i) { return 0; }
	int track_bbox_top(int ch, int i) { return 0; }
	int track_bbox_bottom(int ch, int i) { return 0; }
	int track_bbox_width(int ch, int i) { return 0; }
	int track_bbox_height(int ch, int i) { return 0; }
	float track_angle(int ch, int i) { return 0; }
	int track_major_axis(int ch, int i) { return 0; }
	int track_minor_axis(int ch, int i) { return 0; }
	int track_capture_time() { return 0; }
	int track_previous_capture_time() { return 0; }
	
	int up_button() { return 0; }
	int down_button() { return 0; }
	int left_button() { return 0; }
	int right_button() { return 0; }
	int a_button() { return 0; }
	int b_button() { return 0; }
	int black_button() { return 0; }
	
	void display_clear() { }
	void libcbc_init() { }
}

#endif
