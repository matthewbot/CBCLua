#include "timer.h"
#include "watchdog.h"

#include <sys/time.h>
#include <sys/select.h>
#include <unistd.h>
#include <sched.h>
#include <cstdlib>

#include <vector>
#include <algorithm>

using namespace std;

// high-precision timing functions

unsigned long mseconds() {
	struct timeval time;
	gettimeofday(&time, NULL);
	
	return (unsigned long)time.tv_sec*1000 + (unsigned long)time.tv_usec/1000;
}

double seconds() {
	struct timeval time;
	gettimeofday(&time, NULL);
	
	return (double)time.tv_sec + (double)time.tv_usec/1000000.0;
}

// The raw timing functions put the entire process to sleep

const vector<bool> sleep_select(double seconds, const vector<int> &fds) {
	watchdog_disable();
	
	int highestfd;
	if (fds.size() > 0)
		highestfd = *max_element(fds.begin(), fds.end());
	else
		highestfd = 0;
		
	fd_set fdset_read;
	fd_set fdset_exc;
	FD_ZERO(&fdset_read);
	FD_ZERO(&fdset_exc);
	
	for (vector<int>::const_iterator i = fds.begin(); i != fds.end(); ++i) {
		FD_SET(*i, &fdset_read);
		FD_SET(*i, &fdset_exc);
	}
	
	if (seconds >= 0) {
		struct timeval timeout;
		timeout.tv_sec = (long)seconds;
		timeout.tv_usec = (long)((seconds - timeout.tv_sec) * 1000000);
		
		select(highestfd + 1, &fdset_read, NULL, &fdset_exc, &timeout);
	} else {
		select(highestfd + 1, &fdset_read, NULL, &fdset_exc, NULL);
	}
	
	vector<bool> setvecs;
	setvecs.reserve(fds.size());
	
	for (vector<int>::const_iterator i = fds.begin(); i != fds.end(); ++i) {
		setvecs.push_back(FD_ISSET(*i, &fdset_read) != 0 || FD_ISSET(*i, &fdset_exc) != 0);
	}
		
    watchdog();
    
    return setvecs;
}

