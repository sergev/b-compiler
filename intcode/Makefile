CC      = cc -m32
CFLAGS  = -g -O1 -Wall -Werror
LDFLAGS = -g

PREFIX  = /usr/local

all:    intcode

#
# Intcode interpreter
#
intcode: intcode.o blib.o
	$(CC) $(LDFLAGS) -o intcode intcode.o blib.o

blib.o: blib.c
	$(CC) $(CFLAGS) -c blib.c

intcode.o: intcode.c
	$(CC) $(CFLAGS) -c intcode.c

#
# Installation
#
install: intcode
	install -c -s intcode $(PREFIX)/bin/intcode

clean:
	rm -f OCODE INTCODE ASM *.o *.int intcode
