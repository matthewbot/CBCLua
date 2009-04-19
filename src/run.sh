#!/bin/bash

cd `dirname $0`

export LUA_PATH="mods/?.lua;code/?.lua"
export LUA_CPATH="cmods/?.so"
export LD_LIBRARY_PATH="." # Doesn't work without this for some reason

if [ "$1" == "cbc" ]; then # if we're on a CBC
	LUA_BIN="./lua"
	LUA_START_OPTS="cbc"

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
					echo "cbclua: Waiting for USB drive"
					PRINTEDMSG=1
				fi

				sleep 1
				continue
			fi
			
			break
		fi
		
		echo "cbclua: usb mounted"
		
		if [ -e /mnt/usercode/cbclua.tgz ]; then
			echo "cbclua: applying update"
			rm -rf /mnt/user/code/cbclua
			tar -xzf /mnt/usercode/cbclua.tgz -C /mnt/user/code
			umount /mnt/usercode
			echo "cbclua: done. Please recompile cbclua.c"
			echo "cbclua: (remove cbclua.tgz to stop updating)"
			exit 0
		fi
		
		if [ -d /mnt/usercode/lua ]; then
			echo "cbclua: loading new code"
			rm -rf /mnt/user/code/cbclua/code
			cp -r /mnt/usercode/lua /mnt/user/code/cbclua/code
		fi
		
		umount /mnt/usercode
		
		break
	done
	
	rm /mnt/usercode/notmounted
else
	LUA_BIN="lua"
fi

if [ "$1" == "interact" ]; then
	LUA_OPTS="-i"
fi

$LUA_BIN $LUA_OPTS startup/start.lua $LUA_START_OPTS
