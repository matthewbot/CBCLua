# All paths in this file must be relative to src/cmods/somemodule!

ifeq '$(arch)' 'cbc'
archdir := cbclua
else
archdir := local
endif

rootdir := ../../..

shareddir := $(rootdir)/src/cmods/shared
sharedbuilddir := $(rootdir)/build/$(arch)/shared
sharedobjs := $(patsubst %.cpp,%.o,$(addprefix $(sharedbuilddir)/,$(notdir $(wildcard $(shareddir)/*.cpp))))
sharedheaders := $(wildcard $(shareddir)/*.h)

modbuilddir := $(rootdir)/build/$(arch)/$(modname)
moddir := $(rootdir)/$(archdir)/cmods/$(modpath)
modbin := $(moddir)/$(modname).so
includedir := $(rootdir)/src/include
objects := $(patsubst %.cpp,%.o,$(addprefix $(modbuilddir)/,$(wildcard *.cpp))) $(sharedobjs)
headers := $(wildcard *.h) $(wildcard *.hpp) $(wildcard $(includedir)/*.h) $(wildcard $(includedir)/*.hpp) $(sharedheaders)

cflags += -Wall -pipe -fpic -I$(includedir) -I$(shareddir) -ffast-math -Os
ldflags += -shared -ldl -ffast-math -Os

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
	
$(sharedbuilddir)/%.o: $(shareddir)/%.cpp $(sharedheaders) | $(sharedbuilddir)
	$(CC) -c $< -o $@ $(cflags)
		
$(modbuilddir) $(sharedbuilddir) $(moddir):
	mkdir -p $@
