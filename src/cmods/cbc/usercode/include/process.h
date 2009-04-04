#ifndef __PROCESS_H__
#define __PROCESS_H__

#include <pthread.h>

#define MAX_PROCESSES 16
#define MAX_MUTEXES 8

int  start_process(void (*func)());
void kill_process(int pid);

void *__run_process(void *ptr);
void __init_ptable();

struct __ptable_entry {
	pthread_t thread;
	int in_use;
};

#endif
