#include "timer.h"
#include "watchdog.h"
#include <pthread.h>
#include <unistd.h>
#include <iostream>
#include <cstdlib>

using namespace std;

static volatile long watchdog_lasttime=0L;
static pthread_t watchdogthread;
static volatile bool watchdog_enabled=false;
static volatile bool watchdog_started=false;

extern "C" void *watchdog_func(void *unused) {
	while (true) {
		if (watchdog_enabled && mseconds() - watchdog_lasttime > 500) {
			cerr << "watchdog: timer passed. program is likely stalled" << endl;
			watchdog_enabled = false;
		}	
		
		usleep(200 * 1000); // 100 msecs
	}
	
	return NULL;
}

static void watchdog_start() {
	if (pthread_create(&watchdogthread, NULL, watchdog_func, NULL) != 0) {
		cerr << "watchdog: failed to create watchdog thread" << endl;
		exit(1);
	}
	watchdog_started = true;
}

void watchdog() {
	if (watchdog_started == false)
		watchdog_start();
	
	watchdog_lasttime = mseconds();
	watchdog_enabled = true;
}

void watchdog_disable() {
    watchdog_enabled = false;
}

