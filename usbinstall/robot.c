int main() {
	printf("Installing CBCLua v2!\n");
	
	system("rm -rf /mnt/user/code/cbclua");
	system("tar -xzf /mnt/browser/usb/cbclua.tgz -C /mnt/user/code -p");
	system("sync");
	
	printf("CBCLua installed!\n");
}
