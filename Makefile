happy.nes: src/happy.o src/reset.o src/readjoy.o src/happy_soundtrack.o src/pentlymusic.o \
	src/pentlysound.o src/paldetect.o
	ld65 src/happy.o src/reset.o src/readjoy.o src/happy_soundtrack.o src/pentlymusic.o src/pentlysound.o \
		src/paldetect.o -t nes -o happy.nes

src/happy.o: src/happy.s src/constants.inc src/header.inc assets/graphics.chr assets/board.nam \
	src/pently.inc src/pentlyseq.inc src/pentlyconfig.inc

src/happy_soundtrack.o: src/happy_soundtrack.s src/pentlyseq.inc src/pentlyconfig.inc

%.o: %.s
	ca65 $<

clean:
	rm src/*.o happy.nes
