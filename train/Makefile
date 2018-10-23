all: udsenser.hs EM.hs Matching.hs em_core.o
	ghc --make -O2 udsenser.hs EM.hs Matching.hs em_core.o -lpgf -lgu -lm

em_core.o: em_core.c em_core.h
	gcc -O2 -std=c99 -c em_core.c

Matching.hs: Matching.hsc em_core.h
	hsc2hs --cflag="-std=c99" Matching.hsc
