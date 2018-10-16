all: training.hs em.o
	ghc --make -O2 training.hs em.o -lpgf -lgu -lm

em.o: em.c
	gcc -O2 -std=c99 -c em.c
