#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "serial.h"

#ifdef CBCLUA_CBC

static int rx=0;
static int tx=0;

static void die(const char *msg) {
	fprintf(stderr, "serial: %s\n", msg);
	exit(1);
}

void serial::init() {
	if (rx)
		return;
	
	tx = open("/tmp/uart1tx", O_WRONLY);
	if (tx == -1)
		die("Failed to open /tmp/uart1tx");
	
	rx = open("/tmp/uart1rx", O_RDONLY);
	if (rx == -1)
		die("Failed to open /tmp/uart1rx");
}

void serial::quit() {
	if (!rx)
		return;
	
	close(tx);
	close(rx);
	
	tx=0;
	rx=0;
}

int serial::read(char *buf, int amt) {
	return ::read(rx, buf, amt);
}

bool serial::poll(double timeout) {
	if (rx) {
		struct timeval timeblock;
		timeblock.tv_sec = (long)timeout;
		timeblock.tv_usec = (long)((timeout - timeblock.tv_sec) * 1000000);
	
		fd_set fdset;
		FD_ZERO(&fdset);
		FD_SET(rx, &fdset);
	
		select(rx+1, &fdset, NULL, NULL, &timeblock);
	
		return FD_ISSET(rx, &fdset);
	} else {
		usleep((unsigned long)(timeout * 1000000));
		
		return false;
	}
}

void serial::write(const char *buf, int amt) {
	while (1) {
		int wrote = ::write(tx, buf, amt);
		
		if (wrote == amt)
			return;
		
		amt -= wrote;
		buf += wrote;
	}
}

#else // on PC

void serial::init() { }

void serial::quit() { }

int serial::read(char *buf, int amt) { return 0; }

bool serial::poll(double timeout) {
	usleep((unsigned long)(timeout * 1000000));
	return false;
}

void serial::write(const char *buf, int amt) { }

#endif

