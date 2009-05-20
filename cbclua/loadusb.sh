#!/bin/bash

mkdir -p /mnt/usercode # search for USB drives
touch /mnt/usercode/notmounted
PRINTEDMSG=0

while true; do	
	mount /dev/sdb1 /mnt/usercode -t vfat -o ro 2>/dev/null
	mount /dev/sdc1 /mnt/usercode -t vfat -o ro 2>/dev/null
	mount /dev/sdd1 /mnt/usercode -t vfat -o ro 2>/dev/null
	
	if [ -e /mnt/usercode/notmounted ]; then
		USBCOUNT=`ls /sys/bus/usb/devices | wc -l`
		if [ "$USBCOUNT" == "6" ] || [ "$USBCOUNT" == "10" ]; then # count with and without camera
			if [ "$PRINTEDMSG" == "0" ]; then
				echo "Waiting for USB drive"
				PRINTEDMSG=1
			fi

			sleep 1
			continue
		else
			break
		fi
	fi
	
	if [ -d /mnt/usercode/lua ]; then
		echo "Loading USB code in folder 'lua'"
		rm -rf /mnt/user/code/cbclua/code
		cp -r /mnt/usercode/lua /mnt/user/code/cbclua/code
	fi
	
	umount /mnt/usercode
	
	break
done

rm /mnt/usercode/notmounted
