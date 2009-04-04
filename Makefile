luadir := lua-5.1.4

modules := $(wildcard src/cmods/*)

all: $(modules)

.PHONY: $(modules)
$(modules):
	$(MAKE) -C $@ arch=local
	$(MAKE) -C $@ arch=cbc

.PHONY: clean	
clean:
	rm -rf build usbdrive/cbclua
	
.PHONY: image
image:
	rm -rf usbdrive/cbclua
	cp -rL cbc usbdrive
	mv usbdrive/cbc usbdrive/cbclua
