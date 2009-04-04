# This is what a makefile for a C module should look like!

# modpath place the module in this directory(s) relative to lua. 
# For example, this module will be placed in x86/std, and to lua will be called raw.cbc 
# (std.cbc is a lua module that paints an OO interface over these simple bindings)
# You might want to put your schools letters as the modpath to avoid conflicts with standard modules
modpath:=raw

# modname is the name of the module. All module names must be unique.
modname:=cbc

# Below here is special to the cbc module which needs to be linked to the actual CBC libraries
# Don't include it in your own module!

ifeq '$(arch)' 'cbc'
ldflags += -Lusercode/lib -lcbc -ltrack -lshared
endif

include ../../../shared.mk

