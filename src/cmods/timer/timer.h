#ifndef CBCLUA_TIMER_H
#define CBCLUA_TIMER_H

#include <vector>

unsigned long mseconds(); // returns number of milliseconds
double seconds(); // returns time in seconds to highest precision	

const std::vector<bool> sleep_select(double seconds, const std::vector<int> &fds);

#endif
