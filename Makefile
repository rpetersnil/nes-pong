.DEFAULT_GOAL := nes-pong

%: %.asm
	nesasm $@.asm
#	# Create symboles for FCEUX emulator
#	python fns2nl.py $@.fns > $@.nes.0.nl

all: nes-pong

clean:
	rm -f *.dbg
	rm -f *.cdl
	rm -f *.fns
	rm -f *.nl
	rm -f *.nes

run: nes-pong
	/Applications/Mesen.app/Contents/MacOS/Mesen ${CURDIR}/nes-pong.nes