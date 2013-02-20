
all:    trantest

trantest: ../btran/btran ../gen-intcode/gen-intcode syn.b trn.b blib.b iclib.intcode
	echo '#!/usr/bin/env intcode' > $@
	cat iclib.intcode >> $@
	../btran/btran < syn.b && ../gen-intcode/gen-intcode < OCODE && cat INTCODE >> $@
	../btran/btran < trn.b && ../gen-intcode/gen-intcode < OCODE && cat INTCODE >> $@
	../btran/btran < blib.b && ../gen-intcode/gen-intcode < OCODE && cat INTCODE >> $@
	chmod +x $@

btrantest: bcpl-tran ../gen-intcode/gen-intcode syn.bcpl trn.bcpl blib.bcpl iclib.intcode
	echo '#!/usr/bin/env intcode' > $@
	cat iclib.intcode >> $@
	./bcpl-tran < syn.bcpl && ../gen-intcode/gen-intcode < OCODE && cat INTCODE >> $@
	./bcpl-tran < trn.bcpl && ../gen-intcode/gen-intcode < OCODE && cat INTCODE >> $@
	./bcpl-tran < blib.bcpl && ../gen-intcode/gen-intcode < OCODE && cat INTCODE >> $@
	chmod +x $@

clean:
	rm -f OCODE INTCODE ASM *.o trantest btrantest
