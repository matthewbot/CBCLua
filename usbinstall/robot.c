int main() {
	printf("Installing CBCLua v1!\n");
	
	system("mkdir -p /mnt/tmpusb");
	system("mount /dev/sdb1 /mnt/tmpusb -t vfat -o ro");
	system("rm -rf /mnt/user/code/cbclua");
	system("tar -xzf /mnt/tmpusb/cbclua.tgz -C /mnt/user/code -p");
	system("umount /mnt/tmpusb");
	system("sync");
	
	printf("CBCLua installed!\n");
}
