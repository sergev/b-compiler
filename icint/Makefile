CC      = cc -m32
CFLAGS  = -g -O1 -Wall -Werror
LDFLAGS = -g

PREFIX  = /usr/local

all:    icint

#
# Intcode interpreter
#
icint: icint.o blib.o
	$(CC) $(LDFLAGS) -o icint icint.o blib.o

blib.o: blib.c
	$(CC) $(CFLAGS) -c blib.c

icint.o: icint.c
	$(CC) $(CFLAGS) -c icint.c

#
# Installation
#
install: icint
	-mkdir $(PREFIX)/lib/bcpl
	install -c icint $(PREFIX)/lib/bcpl/icint

clean:
	rm -f OCODE INTCODE ASM *.o *.int icint
