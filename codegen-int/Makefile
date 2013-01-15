AS              = cc -m32 -c
AFLAGS          = -g
CC              = cc -m32
CFLAGS          = -g -O1 -Wall -Werror
LD              = ld -melf_i386
LDFLAGS         = -g

PREFIX          = /usr/local

all:    	codegen-int

#
# Intcode generator, native
#
codegen-int:    codegen-int.o
		$(CC) $(LDFLAGS) -o $@ $<

clean:
		rm -f OCODE INTCODE ASM *.o *.int codegen-int
