#ifndef CBCMOD_SERIAL_H
#define CBCMOD_SERIAL_H

namespace serial {
	void init();
	void quit();
	
	int read(char *buf, int amt);
	bool poll(double timeout);
	void write(const char *buf, int amt);
}





#endif
