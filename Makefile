hyperloop.nes: hyperloop.s
	ca65 hyperloop.s -o hyperloop.o -t nes
	ld65 hyperloop.o -o hyperloop.nes -t nes

clean:
	rm *.o *.nes