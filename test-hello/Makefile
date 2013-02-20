
all:    hello

hello:  ../btran/btran ../gen-intcode/gen-intcode hello.b iclib.intcode
	../btran/btran < hello.b
	../gen-intcode/gen-intcode < OCODE
	echo '#!/usr/bin/env intcode' > $@
	cat iclib.intcode INTCODE >> $@
	chmod +x $@

bhello: bcpl-tran ../gen-intcode/gen-intcode hello.bcpl iclib.intcode
	./bcpl-tran < hello.bcpl
	../gen-intcode/gen-intcode < OCODE
	echo '#!/usr/bin/env intcode' > $@
	cat iclib.intcode INTCODE >> $@
	chmod +x $@

clean:
	rm -f OCODE INTCODE ASM *.o hello bhello
