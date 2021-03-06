#!/bin/bash

echo "CBCLua 2 loading..."

cd `dirname $0`

export CBCLUA_CODEPATH="code/"
export CBCLUA_MODSPATH="mods/"
export LD_LIBRARY_PATH="." # Doesn't work without this for some reason

if [ `uname -r` == "2.6.16-csb" ]; then
	HOST="chumby"
	export CBCLUA_NAME_FILE="/mnt/user/config/cbclua_name"
else
	HOST="pc"
	export CBCLUA_NAME_FILE="cbclua_name"
fi

if [ "$HOST" == "chumby" ]; then # if we're on a chumby
	LUA_BIN="./lua"
	export LUA_PATH="stdmods/?.lua"
	export LUA_CPATH="cmods/?.so;code/?.so"
else
	LUA_BIN="lua"
	export LUA_PATH="stdmods/?.lua;;"
	export LUA_CPATH="cmods/?.so;code/?.so;;"
fi

if [ "$1" == "gdb" ]; then
	LUA_BIN="gdb --args $LUA_BIN"
fi

exec $LUA_BIN startup/start.lua $HOST # start cbclua

