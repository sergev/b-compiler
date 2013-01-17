CC      = cc -m32
CFLAGS  = -g -O1 -Wall -Werror
LDFLAGS = -g

PREFIX  = /usr/local

all:    btran.o

#
# B-to-ocode translator
#
btran:  btran.o blib.o
	$(CC) $(LDFLAGS) -o btran btran.o blib.o

#
# Installation
#
install: btran
	-mkdir $(PREFIX)/lib/bcpl
	install -c btran $(PREFIX)/lib/bcpl/btran

clean:
	rm -f OCODE INTCODE ASM *.o *.int btran
