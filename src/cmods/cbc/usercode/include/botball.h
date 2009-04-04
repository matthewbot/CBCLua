#ifndef __BOTBALL_H__
#define __BOTBALL_H__

#include <stdio.h>

#include "compat.h"

void wait_for_light(int light_port_);
void shut_down_in(float delay);
void _shut_down_task();

#endif
