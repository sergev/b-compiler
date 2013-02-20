
all:    cmpltest

cmpltest: ../btran/btran ../gen-intcode/gen-intcode cmpltest.b iclib.intcode
	../btran/btran < cmpltest.b
	../gen-intcode/gen-intcode < OCODE
	echo '#!/usr/bin/env intcode' > $@
	cat iclib.intcode INTCODE >> $@
	chmod +x $@

bcmpltest: bcpl-tran ../gen-intcode/gen-intcode cmpltest.bcpl iclib.intcode
	./bcpl-tran < cmpltest.bcpl
	../gen-intcode/gen-intcode < OCODE
	echo '#!/usr/bin/env intcode' > $@
	cat iclib.intcode INTCODE >> $@
	chmod +x $@

clean:
	rm -f OCODE INTCODE ASM *.o cmpltest bcmpltest
