CC              = cc -m32
CFLAGS          = -g -O1 -Wall -Werror
LDFLAGS         = -g

PREFIX          = /usr/local

all:    	gen-intcode

#
# Intcode generator, native
#
gen-intcode:    gen-intcode.o
		$(CC) $(LDFLAGS) -o $@ $<

clean:
		rm -f OCODE INTCODE ASM *.o *.int gen-intcode
