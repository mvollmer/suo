CFLAGS=-g

suo: suo-runtime.o suo-glue.o
	$(CC) -g -o suo suo-runtime.o suo-glue.o

clean:
	rm -f suo *.o
