# This is what a makefile for a C module should look like!

# modpath place the module in this directory(s) relative to lua. 
# For example, lua will call this module raw.cbc 
# (std.cbc is a lua module that paints an OO interface over these simple bindings)
# You might want to put your schools letters as the modpath to avoid conflicts with standard modules
modpath:=raw

# modname is the name of the module. All module names must be themselves be unique.
# (even if in different modpaths)
modname:=io

# This line must be included in the Makefile. Otherwise it won't do anything
include ../../../shared.mk
