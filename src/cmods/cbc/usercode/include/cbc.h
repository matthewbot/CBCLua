//CBC user library functions
//--DPM 12/27/08

//Note that button functions and display function have not been updated yet (see end of file)
//Note that voltage and sonar scaling probably need adjustment

////////////////////////////////////////////////////////////////////////////////////////////
// Function prototypes
/* Includes the Standard IO Library */
#ifndef __CBC_H__
#define __CBC_H__

#include <stdio.h>
#include "compat.h"

extern int __pid_defaults[6];

void tone(int frequency, int duration); /* makes a sound at frequency for duration ms */
void beep(); /* make a beep */
int digital(int port); /* returns a 1 or 0 reflecting the state of port (0 to 7) */
int set_digital_output_value(int port, int value); /*sets port (0 to 7)to value (0 or 1) */
int analog10(int port); /* returns 10-bit value from analog port (ports 8 to 15) */
int analog(int port); /* returns 8-bit value from analog port (ports 8 to 15) */
int accel_x(); /* returns x accelleration (-2047 to 2047, +/- 1.5 gee) */
int accel_y(); /* returns y accelleration (-2047 to 2047, +/- 1.5 gee) */
int accel_z(); /* returns z accelleration (-2047 to 2047, +/- 1.5 gee) */
int sonar(int port); /* returns range in mm for sonar plugged into port (13-15)*/
int sonar_inches(int port); /* returns range in whole inches for sonar plugged into port (13-15)*/
float power_level(); /* returns a float battery voltage */
void enable_servos(); /* powers up the servos */
void disable_servos(); /* powers down the servos */
int set_servo_position(int servo, int pos); /* sets servo (1 to 4) to pos (0 to 2047) */
int get_servo_position(int servo); /* returns int of last setting for servo (1 to 4) */
int get_motor_position_counter(int motor); /* returns int of motor (0 to 3) position +/-2147483647 */
int clear_motor_position_counter(int motor); /* sets motor (0 to 3) counter to 0 */
int move_at_velocity(int motor, int velocity); /* PID control of motor (0 to 3) at velocity tick per second */
int mav(int motor, int velocity); /* PID control of motor (0 to 3) at velocity tick per second */
int move_to_position(int motor, int speed, int goal_pos);/* move motor (0 to 3) at speed to goal_pos */
int mtp(int motor, int speed, int goal_pos);/* move motor (0 to 3) at speed to goal_pos */
int move_relative_position(int motor, int speed, int delta_pos);/* move motor (0 to 3) at speed by delta_pos */
int mrp(int motor, int speed, int delta_pos);/* move motor (0 to 3) at speed by delta_pos */
void set_pid_gains(int motor, int p, int i, int d, int pd, int id, int dd);/* set PID gains */
int freeze(int motor);/* keep motor (0 to 3) at current position */
int get_motor_done(int motor); /* returns 1 if motor (0 to 3) is moving to a goal and 0 otherwise */
void block_motor_done(int motor); /* returns when motor (0 to 3) has reached goal */
void bmd(int motor); /* returns when motor (0 to 3) has reached goal */
int setpwm(int motor, int pwm); /* turns on motor (0 to 3) at pwm (-255 to 255)*/
int getpwm(int motor);/* retruns the current pwm setting for that motor (-255 to 255)*/
void fd(int motor); /* motor (0 to 3) at full forward */
void bk(int motor); /* motor (0 to 3) at full reverse */
void motor (int motor, int percent);/* motor (0 to 3) at percent % of full (-100 to 100)*/
void off(int motor); /* turns motor (0 to 3) off */
void ao(); /* turns all motors off */

int up_button();
int down_button();
int left_button();
int right_button();
int a_button();
int b_button();

void kissSim_init(int world, int rx, int ry, int rt);
void kissSim_end();
void kissSimEnablePause();
void kissSimPause();
int kissSimActive();
int black_button();/* returns value of hardware button on CBC */
void display_clear(); /* Clears display and pust cursor in upper left*/
void cbc_display_clear();/*Clears console display on CBC*/
void cbc_printf(int col, int row, char *t, ...);/*Does printf at col,row */

#define SIMPLEWORLD 0
#define BB08WORLD 1
#define EMPTYWORLD 2
#define RULERWORLD 3
#define BIGEMPTYWORLD 4
#define BIGARENA 5
#define BIGARENA2 6
#define BB09WORLD 7


#endif
