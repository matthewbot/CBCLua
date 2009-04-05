#include "timer.h"
#include <pthread.h>
#include <unistd.h>
#include <iostream>
#include <cstdlib>

using namespace std;

static long watchdog_lasttime=0L;
static pthread_t watchdogthread;
static bool watchdog_enabled=false;
static bool watchdog_started=false;

static void *watchdog_func(void *unused) {
	while (1) {
		usleep(100 * 1000); // 100 msecs
		if (watchdog_enabled && raw_mseconds() - watchdog_lasttime > 500) {
			printf("watchdog: timer passed. program is likely stalled.\n");
			watchdog_enabled = false;
		}	
	}
	
	return NULL; // stupid GCC warning
}

static void watchdog_start() {
	if (pthread_create(&watchdogthread, NULL, watchdog_func, NULL) != 0) {
		cerr << "watchdog: failed to create watchdog thread" << endl;
		exit(1);
	}
	
	watchdog_started = true;
}

void watchdog() {
	if (!watchdog_started)
		watchdog_start();
	
	watchdog_lasttime = raw_mseconds();
	watchdog_enabled = true;
}

