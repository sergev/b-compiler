
NO_WARNINGS = -O -Werror \
              -Wno-multichar \
              -Wno-implicit-int \
              -Wno-implicit-function-declaration \
              -Wno-unused-result

all:    b hello.s b.s

b:      ../tools/b.c
	gcc $(NO_WARNINGS) $< -o $@

clean:
	rm -f b

hello.s: b ../hello.b
	./b ../hello.b hello.s

b.s: b  ../b.b
	./b ../b.b b.s
