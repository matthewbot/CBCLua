# This is what a makefile for a C module should look like!

# modpath place the module in this directory(s) relative to lua. 
# For example, lua will call this module cbclua.rawcbc
# You might want to put your schools letters as the modpath to avoid conflicts
modpath:=cbclua

# modname is the name of the module. All C module names must be themselves be unique 
# for the build system to work correctly. (even if in different modpaths)
modname:=rawcbc

# This line must be included in the Makefile. Otherwise it won't do anything
include ../../../shared.mk

# Below here is special to the cbc module which needs to be linked to the actual CBC libraries
# Don't include it in your own module!

ifeq '$(arch)' 'cbc'
ldflags += -Lusercode/lib -lcbc -ltrack -lshared
endif
