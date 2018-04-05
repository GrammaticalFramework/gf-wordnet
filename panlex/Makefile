levenstein.c: translit.txt
	runghc translit.hs

levenstein.so: levenstein.c
	gcc -std=c99 -g -fPIC -shared levenstein.c -o levenstein.so

panlex.db: levenstein.so import.sql
	sqlite3 <import.sql
