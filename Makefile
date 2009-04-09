luadir := lua-5.1.4

modules := $(wildcard src/cmods/*)

all: $(modules) interact

.PHONY: $(modules)
$(modules):
	$(MAKE) -C $@ arch=local
	$(MAKE) -C $@ arch=cbc

.PHONY: clean	
clean:
	rm -rf build usbdrive/cbclua
	
.PHONY: usbinstall
usbinstall:
	rm -rf usbinstall/cbclua
	cp -rL cbc usbinstall/cbclua

.PHONY: interact
interact:
	$(MAKE) -C src/interact
