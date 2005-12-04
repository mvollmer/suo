CFLAGS=-g

suo: suo-runtime.o
	$(CC) -g -o suo suo-runtime.o
