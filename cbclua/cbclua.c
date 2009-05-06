// This is the launcher program compiled on the CBC
// It allows users to run cbclua through the cbcui

#include <unistd.h>

int main(int argc, char **argv) {
	execl("/mnt/user/code/cbclua/run.sh", "cbcconsole", (char *)0); // exec replaces the current process with a new one
	
	fprintf(stderr, "Failed to exec run.sh! Please re-install cbclua\n");
}
