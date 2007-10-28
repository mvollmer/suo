CFLAGS=-g

suo: suo-runtime.o suo-glue.o
	$(CC) -g -o suo suo-runtime.o suo-glue.o

suo-vm: suo-vm.o suo-glue.o
	$(CC) -g -o suo-vm suo-vm.o suo-glue.o

clean:
	rm -f suo suo-vm *.o
