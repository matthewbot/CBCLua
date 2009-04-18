// This is the launcher program compiled on the CBC
// It allows users to run cbclua through the cbcui

const char *errmsg = "\
Unable to launch cbclua, error %d.\n\
You should probably do a full\n\
firmware reinstall.\
";

int main(int argc, char **argv) {
	int status = system("/mnt/user/cbclua/run.sh");

	if (status != 0) {
		fprintf(stderr, errmsg, status);
		return 1;
	}

	return 0;
}
