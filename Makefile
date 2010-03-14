luadir := lua-5.1.4

modules := $(filter-out %shared, $(wildcard src/cmods/*))
cbc-modules := $(addprefix cbc-, $(modules))

all: $(modules)

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
	rm -rf build local/cmods/* cbclua/cmods/* cbclua/lua src/lua-5.1.4-arm/*.o src/luasocket-2.0.2-arm/src/*.o doc
	
.PHONY: usbinstall
usbinstall: cbc
	rm -rf usbinstall/cbclua.tgz
	tar -czf usbinstall/cbclua.tgz cbclua --exclude=".*" --exclude="*~" -h -p

.PHONY: wifiinstall
wifiinstall: cbc
	./wifiinstall.sh
	
.PHONY: doc
doc:
	rm -rf doc
	luadoc --nofiles -d doc/cbclua src/mods
	
.PHONY: cbc-lua-binary
cbc-lua-binary:
	$(MAKE) -C src/lua-5.1.4-arm
	
.PHONY: cbc-luasocket-binary
cbc-luasocket-binary:
	$(MAKE) -C src/luasocket-2.0.2-arm
	mkdir -p cbclua/cmods/socket cbclua/cmods/mime
	cp src/luasocket-2.0.2-arm/src/socket.so.* cbclua/cmods/socket/core.so
	cp src/luasocket-2.0.2-arm/src/mime.so.* cbclua/cmods/mime/core.so
