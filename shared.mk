# All paths in this file must be relative to src/cmods/somemodule!

rootdir := ../../..
modbuilddir := $(rootdir)/build/$(arch)/$(modname)
moddir := $(rootdir)/$(arch)/cmods/$(modpath)
modbin := $(moddir)/$(modname).so
includedir := $(rootdir)/src/include
objects := $(patsubst %.cpp,%.o,$(addprefix $(modbuilddir)/,$(wildcard *.cpp)))
headers := $(wildcard *.h) $(wildcard *.hpp) $(wildcard $(includedir)/*.h) $(wildcard $(includedir)/*.hpp)

cflags += -Wall -pipe -fpic -I$(includedir)
ldflags += -shared -ldl

ifeq '$(arch)' 'local'
CC := g++
LD := g++
cflags += -O2 -DCBCLUA_COMP
else
CC := arm-linux-g++
LD := arm-linux-g++
cflags += -Os -DCBCLUA_CBC -mcpu=arm926ej-s
endif



all: $(modbin)

$(modbin): $(objects) | $(moddir)
	$(LD) -o $(modbin) $(objects) $(ldflags)
	
$(modbuilddir)/%.o: %.cpp $(headers) | $(modbuilddir)
	$(CC) -c $< -o $@ $(cflags)
		
$(modbuilddir) $(moddir):
	mkdir -p $@
