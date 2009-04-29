#!/bin/bash

echo "cbclua: v0.3 loading..."

MODE=$1
cd `dirname $0`

export LUA_PATH="code/?.lua;code/?/mod.lua;mods/?.lua;mods/?/main.lua"
export LUA_CPATH="code/?.so;cmods/?.so"
export LD_LIBRARY_PATH="." # Doesn't work without this for some reason

if [ `uname -r` == "2.6.16-csb" ]; then
	LUA_BIN="./lua"
else
	LUA_BIN="lua"
fi

if [ "$MODE" == "cbcconsole" ]; then # if we're being run from the console
	./loadusb.sh # do USB loading stuffs
fi

$LUA_BIN startup/start.lua $MODE
