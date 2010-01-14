#!/bin/bash

echo "CBCLua 2 loading..."
echo "Developed at Nease High School, FL"

cd `dirname $0`

export LUA_PATH="mods/?.lua;;"
export LUA_CPATH="code/?.so;cmods/?.so;;"
export CBCLUA_CODEPATH="code/"
export CBCLUA_MODPATH="mods/"
export LD_LIBRARY_PATH="." # Doesn't work without this for some reason

if [ `uname -r` == "2.6.16-csb" ]; then
	HOST="chumby"
else
	HOST="pc"
fi

if [ "$HOST" == "chumby" ]; then # if we're on a chumby
	if [ `iwconfig 2>&1 | grep WLAN | wc -l` == "0" ]; then # if theres no wifi plugged in
		./loadusb.sh # do USB loading stuffs
	fi
	
	LUA_BIN="./lua"
else
	LUA_BIN="lua"
fi

if [ "$1" == "gdb" ]; then
	LUA_BIN="gdb --args $LUA_BIN"
fi

exec $LUA_BIN startup/start.lua $HOST # start cbclua
