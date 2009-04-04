#ifndef __COMPAT_H__
#define __COMPAT_H__

#include "process.h"
#include <unistd.h>

#include <time.h>

void fsleep(float ftime);
void msleep(long mtime);

float seconds();

#define sleep(x) fsleep(x)

#endif

