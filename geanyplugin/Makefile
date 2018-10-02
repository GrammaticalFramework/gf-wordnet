wordnetplugin.so: wordnetplugin.o jsmn.o
	gcc wordnetplugin.o jsmn.o -o wordnetplugin.so -shared `pkg-config --libs geany`

wordnetplugin.o: wordnetplugin.c jsmn.h
	gcc -O2 -c wordnetplugin.c -fPIC `pkg-config --cflags geany`

jsmn.o: jsmn.c jsmn.h
	gcc -O2 -c jsmn.c -fPIC

install: wordnetplugin.so
	install wordnetplugin.so `pkg-config --variable=libdir geany`/geany
