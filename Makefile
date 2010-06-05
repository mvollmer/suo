all: suo suo-dbg

x.o: suo-runtime.c
	gcc -std=gnu99 -fomit-frame-pointer -O3 -g -c -o x.o suo-runtime.c

x.S: suo-runtime.c
	gcc -std=gnu99 -fomit-frame-pointer -O3 -S -o x.S suo-runtime.c

x.s: x.o
	objdump --disassemble x.o >x.s

suo: suo-runtime.c
	gcc -std=gnu99 -g -O3 -o $@ suo-runtime.c

suo-dbg: suo-runtime.c
	gcc -DDEBUG -std=gnu99 -g -o $@ suo-runtime.c

clean:
	rm -f *.o suo suo-dbg
