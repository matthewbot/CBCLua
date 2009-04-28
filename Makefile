luadir := lua-5.1.4

modules := $(filter-out %shared, $(wildcard src/cmods/*))

all: $(modules) interact

.PHONY: $(modules)
$(modules):
	$(MAKE) -C $@ arch=local
	$(MAKE) -C $@ arch=cbc

.PHONY: clean	
clean:
	rm -rf build usbdrive/userhook0_data.tgz local/Interact local/ui.glade src/interact/*.hi src/interact/*.o src/interact/Interact
	
.PHONY: usbinstall
usbinstall: $(modules)
	rm -rf usbinstall/cbclua.tgz
	tar -czf usbinstall/cbclua.tgz cbclua --exclude=".*" --exclude="*~" -h -p

.PHONY: wifiinstall
wifiinstall: $(modules)
	./wifiinstall.sh

.PHONY: interact
interact:
	$(MAKE) -C src/interact
