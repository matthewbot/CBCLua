// This is the launcher program compiled on the CBC
// It allows users to run cbclua through the cbcui

#include <unistd.h>

int main(int argc, char **argv) {
	execl("/bin/sh", "sh", "/mnt/user/code/cbclua/run.sh"); // exec replaces the current process with a new one
	
	fprintf(stderr, "Failed to exec run.sh! Please re-install cbclua\n");
}
