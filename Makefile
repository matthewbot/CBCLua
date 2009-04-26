luadir := lua-5.1.4

modules := $(filter-out %shared, $(wildcard src/cmods/*))

all: $(modules) interact

.PHONY: $(modules)
$(modules):
	$(MAKE) -C $@ arch=local
	$(MAKE) -C $@ arch=cbc

.PHONY: clean	
clean:
	rm -rf build usbdrive/userhook0_data.tgz
	
.PHONY: usbinstall
usbinstall:
	rm -rf usbinstall/cbclua.tgz
	tar -czfp usbinstall/cbclua.tgz cbclua --exclude=".*" --exclude="*~" -h

.PHONY: interact
interact:
	$(MAKE) -C src/interact
