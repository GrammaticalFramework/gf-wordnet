all: udsenser.hs EM.hs em_core.o
	ghc --make -O2 udsenser.hs EM.hs em_core.o -lpgf -lgu -lm

em_core.o: em_core.c
	gcc -g3 -std=c99 -c em_core.c
