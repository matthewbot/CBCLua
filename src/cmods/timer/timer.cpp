#include "timer.h"

#include <sys/time.h>
#include <lua.hpp>

// raw functions just return values since the epoch

static unsigned long raw_mseconds() {
	struct timeval time;
	gettimeofday(&time, NULL);
	
	return (unsigned long)time.tv_sec*1000 + (unsigned long)time.tv_usec/1000;
}

static double raw_seconds() {
	struct timeval time;
	gettimeofday(&time, NULL);
	
	return (double)time.tv_sec + (double)time.tv_usec/1000000.0;
}

// the "real" functions return time since last reset

static long mseconds_start;
static double seconds_start;

unsigned long mseconds() { return raw_mseconds() - mseconds_start; }
double seconds() { return raw_seconds() - seconds_start; }
void reset() {
	mseconds_start = raw_mseconds();
	seconds_start = raw_seconds();
}
