#ifndef CBCLUA_TIMER_H
#define CBCLUA_TIMER_H

unsigned long mseconds(); // returns number of milliseconds
double seconds(); // returns time in seconds to highest precision
	
void reset(); // resets the timer

#endif
