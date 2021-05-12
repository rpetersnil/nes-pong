.DEFAULT_GOAL := nes-pong

%: %.asm
	nesasm $@.asm
	python fns2nl.py $@.fns > $@.nes.0.nl

clean:
	rm -f *.dbg
	rm -f *.cdl
	rm -f *.fns
	rm -f *.nl
	rm -f *.nes
