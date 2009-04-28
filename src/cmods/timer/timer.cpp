#include "timer.h"
#include "watchdog.h"

#include <sys/time.h>
#include <unistd.h>
#include <sched.h>
#include <cstdlib>

using namespace std;

// raw functions just return values since the epoch

unsigned long raw_mseconds() {
	struct timeval time;
	gettimeofday(&time, NULL);
	
	return (unsigned long)time.tv_sec*1000 + (unsigned long)time.tv_usec/1000;
}

double raw_seconds() {
	struct timeval time;
	gettimeofday(&time, NULL);
	
	return (double)time.tv_sec + (double)time.tv_usec/1000000.0;
}

// the "real" functions return time since last reset

static long mseconds_start;
static double seconds_start;

unsigned long mseconds() { return raw_mseconds() - mseconds_start; }
double seconds() { return raw_seconds() - seconds_start; }
void starttime() {
	mseconds_start = raw_mseconds();
	seconds_start = raw_seconds();
}

// The raw timing functions put the entire process to sleep

void rawsleep(double secs) {
	watchdog_disable();
    usleep((unsigned long)(secs * 1000000));
    watchdog();
}

void rawyield() {
	watchdog_disable();
	sched_yield();
	watchdog();
}

