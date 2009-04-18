#!/bin/sh

cd `dirname "$0"`

export LUA_PATH="mods/?.lua;code/?.lua"
export LUA_CPATH="cmods/?.so"
export LD_LIBRARY_PATH="." # Doesn't work without this for some reason

./lua mods/start.lua