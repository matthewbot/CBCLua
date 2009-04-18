#!/bin/sh

cd `dirname $0`

export LUA_PATH="mods/?.lua;code/?.lua"
export LUA_CPATH="cmods/?.so"
export LD_LIBRARY_PATH="." # Doesn't work without this for some reason

if [ -f ./lua ]; then
	LUA_BIN="./lua"
else
	LUA_BIN=`which lua`
	if [ ! -n "$LUA_BIN" ]; then
		echo "Unable to find lua!"
		exit 1
	fi
fi

$LUA_BIN startup/start.lua
