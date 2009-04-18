#include <unistd.h>

const char *cbclua_run_path = "/mnt/user/cbclua/run.sh";
const char *cbclua_run_args[] = { cbclua_run_path, NULL };

int main(int argc, char **argv) {
	execv(cbclua_run_path, cbclua_run_args);
	
	fprintf(stderr, "Failed to start cbclua!\nYou will probably need to re-install.");
	return 1;
}
#include <unistd.h>

const char *cbclua_run_path = "/mnt/user/cbclua/run.sh";
const char *cbclua_run_args[] = { cbclua_run_path, NULL };

int main(int argc, char **argv) {
	execv(cbclua_run_path, cbclua_run_args);
	
	fprintf(stderr, "Failed to start cbclua!\nYou will probably need to re-install.");
	return 1;
}
