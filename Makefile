happy.nes: src/happy.o src/reset.o src/readjoy.o
	ld65 src/happy.o src/reset.o src/readjoy.o -t nes -o happy.nes

src/happy.o: src/happy.asm assets/graphics.chr

%.o: %.asm
	ca65 $<
