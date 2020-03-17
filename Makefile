happy.nes: src/happy.o src/reset.o src/readjoy.o src/rand.o
	ld65 src/happy.o src/reset.o src/readjoy.o src/rand.o -t nes -o happy.nes

src/happy.o: src/happy.asm src/constants.inc src/header.inc assets/graphics.chr assets/board.nam

%.o: %.asm
	ca65 $<
