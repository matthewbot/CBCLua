luadir := lua-5.1.4

modules := $(filter-out %shared, $(wildcard src/cmods/*))
cbc-modules := $(addprefix cbc-, $(modules))

all: $(modules) interact

.PHONY: local
local: $(modules)

.PHONY: cbc
cbc: $(cbc-modules) cbc-lua-binary cbc-luasocket-binary

.PHONY: $(modules)
$(modules):
	$(MAKE) -C $@ arch=local

.PHONY: $(cbc-modules)
$(cbc-modules):
	$(MAKE) -C $(subst cbc-, , $@) arch=cbc

.PHONY: clean	
clean:
	rm -rf build usbdrive/userhook0_data.tgz local/Interact local/ui.glade local/cmods/* cbclua/cmods/* cbclua/lua src/interact/*.hi src/interact/*.o src/interact/Interact src/lua-5.1.4-arm/*.o
	
.PHONY: usbinstall
usbinstall: cbc
	rm -rf usbinstall/cbclua.tgz
	tar -czf usbinstall/cbclua.tgz cbclua --exclude=".*" --exclude="*~" -h -p

.PHONY: wifiinstall
wifiinstall: cbc
	./wifiinstall.sh

.PHONY: interact
interact:
	$(MAKE) -C src/interact
	
.PHONY: cbc-lua-binary
cbc-lua-binary:
	$(MAKE) -C src/lua-5.1.4-arm
	
.PHONY: cbc-luasocket-binary
cbc-luasocket-binary:
	$(MAKE) -C src/luasocket-2.0.2-arm
	cp src/luasocket-2.0.2-arm/src/socket.so.* cbclua/cmods/socket/core.so
	cp src/luasocket-2.0.2-arm/src/mime.so.* cbclua/cmods/mime/core.so
