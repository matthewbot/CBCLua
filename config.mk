# Change the below value to set the clfags to includethe directory where 
# your system's lua.h and friends are located
# (Or adjust the pkg-config if lua is installed as something other than lua5.1)

CONF_LOCAL_LUA_CFLAGS := $(shell pkg-config lua5.1 --cflags)