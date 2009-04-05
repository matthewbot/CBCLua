#include "timer.h"
#include <lua.hpp>

#include <sys/time.h>
#include <pthread.h>
#include <unistd.h>
#include <iostream>
#include <cstdlib>

using namespace std;

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

// watchdog functions

static long watchdog_lasttime=0L;
static pthread_t watchdogthread;
static bool watchdog_enabled=false;
static bool watchdog_started=false;

static void *watchdog_func(void *unused) {
	while (1) {
		usleep(100000);
		if (watchdog_enabled && raw_mseconds() - watchdog_lasttime > 500) {
			printf("watchdog: timer passed. program is likely stalled.\n");
			watchdog_enabled = false;
		}	
	}
	
	return NULL; // stupid GCC warning
}

void watchdog() {
	if (!watchdog_started) {
		watchdog_started = true;
		if (pthread_create(&watchdogthread, NULL, watchdog_func, NULL) != 0) {
			cerr << "watchdog: failed to create watchdog thread" << endl;
			exit(1);
		}
	}
	
	watchdog_lasttime = raw_mseconds();
	watchdog_enabled = true;
}
