#!/bin/bash

echo "CBCLua v0.8 loading..."
echo "Developed at Nease High School, FL"

cd `dirname $0`

export LUA_PATH="code/?.lua;code/?/mod.lua;mods/?.lua;mods/?/mod.lua"
export LUA_CPATH="code/?.so;cmods/?.so"
export LD_LIBRARY_PATH="." # Doesn't work without this for some reason

if [ `uname -r` == "2.6.16-csb" ]; then
	HOST="chumby"
else
	HOST="pc"
fi

if [ "$1" == "interact" ]; then
	MODE="interact"
else
	MODE="console"
fi

if [ "$HOST" == "chumby" ] && [ "$MODE" == "console" ]; then # if we're being run from the console
	if [ `iwconfig 2>&1 | grep WLAN | wc -l` == "0" ]; then # if theres no wifi plugged in
		./loadusb.sh # do USB loading stuffs
	fi
fi

if [ "$HOST" == "chumby" ]; then
	LUA_BIN="./lua"
else
	LUA_BIN="lua"
fi

exec $LUA_BIN startup/start.lua $HOST $MODE # start cbclua
