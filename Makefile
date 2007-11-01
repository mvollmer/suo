CFLAGS=-g

all: suo suo-vm

suo: suo-runtime.o suo-glue.o
	$(CC) -g -o suo suo-runtime.o suo-glue.o

suo-vm: suo-vm.o
	$(CC) -g -o suo-vm suo-vm.o

clean:
	rm -f suo suo-vm *.o
