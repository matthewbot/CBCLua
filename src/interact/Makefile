all: ../../local/Interact ../../local/ui.glade

../../local/% : %
	cp $< $@ -f

Interact: $(wildcard *.hs)
	ghc --make Main -o Interact
