# This is what a makefile for a C module should look like!

# modpath place the module in this directory(s) relative to lua. 
# For example, this module will be placed in x86/std, and to lua will be called std.cbc
# You might want to put your schools letters as the modpath to avoid conflicts with standard modules
modpath:=std

# modname is the name of the module. All module names must be unique.
modname:=timer

# This line isn't needed for most modules. This module needs it to use the linux threading library, pthread
ldflags += -lpthread

include ../../../shared.mk

