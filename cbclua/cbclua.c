// This is the launcher program compiled on the CBC
// It allows users to run cbclua through the cbcui

const char *errmsg = "\
Unable to launch lua, error %d.\n\
You probably need to reinstall\
CBCLua through its userhook0\
";

int main(int argc, char **argv) {
	printf("cbclua: v0.1 loading\n");
	int status = system("/mnt/user/code/cbclua/run.sh");

	if (status != 0) {
		fprintf(stderr, errmsg, status);
		return 1;
	}

	return 0;
}
