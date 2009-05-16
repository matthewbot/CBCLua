# All paths in this file must be relative to src/cmods/somemodule!

ifeq '$(arch)' 'cbc'
archdir := cbclua
else
archdir := local
endif

rootdir := ../../..

include $(rootdir)/config.mk

shareddir := $(rootdir)/src/cmods/shared
sharedbuilddir := $(rootdir)/build/$(arch)/shared
sharedobjs := $(patsubst %.cpp,%.o,$(addprefix $(sharedbuilddir)/,$(notdir $(wildcard $(shareddir)/*.cpp))))
sharedheaders := $(wildcard $(shareddir)/*.h)

modbuilddir := $(rootdir)/build/$(arch)/$(modname)
moddir := $(rootdir)/$(archdir)/cmods/$(modpath)
modbin := $(moddir)/$(modname).so
includedir := $(rootdir)/src/include
archincludedir := $(rootdir)/src/include-$(arch)
objects := $(patsubst %.cpp,%.o,$(addprefix $(modbuilddir)/,$(wildcard *.cpp))) $(sharedobjs)
headers := $(wildcard *.h) $(wildcard *.hpp) $(wildcard $(includedir)/*.h) $(wildcard $(includedir)/*.hpp) $(sharedheaders)

cflags += -Wall -pipe -fpic -I$(includedir) -I$(archincludedir) -I$(shareddir)
ldflags += -shared -ldl

ifeq '$(arch)' 'local'
CC := g++
LD := g++
STRIP := strip
cflags += -O2 -DCBCLUA_COMP $(CONF_LOCAL_LUA_CFLAGS)
ldflags += -O2
else
CC := arm-linux-g++
LD := arm-linux-g++
STRIP := arm-linux-strip
cflags += -O3 -DCBCLUA_CBC -mcpu=arm926ej-s -ffast-math
ldflags += -ffast-math -O3
endif

all: $(modbin)

$(modbin): $(objects) | $(moddir)
	$(LD) -o $(modbin) $(objects) $(ldflags)
	$(STRIP) $(modbin)
	
$(modbuilddir)/%.o: %.cpp $(headers) | $(modbuilddir)
	$(CC) -c $< -o $@ $(cflags)
	
$(sharedbuilddir)/%.o: $(shareddir)/%.cpp $(sharedheaders) | $(sharedbuilddir)
	$(CC) -c $< -o $@ $(cflags)
		
$(modbuilddir) $(sharedbuilddir) $(moddir):
	mkdir -p $@
