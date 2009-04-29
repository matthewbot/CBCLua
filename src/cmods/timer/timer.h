#ifndef CBCLUA_TIMER_H
#define CBCLUA_TIMER_H

#include <vector>

unsigned long mseconds(); // returns number of milliseconds
double seconds(); // returns time in seconds to highest precision	
void start_timers(); // starts the above functions at 0

unsigned long raw_mseconds(); // raw versions measure since the epoch instead of program start
double raw_seconds();

const std::vector<bool> raw_sleep(double seconds, const std::vector<int> &fds);
void raw_yield();

#endif
