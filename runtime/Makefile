.PHONY: test
test: test_malloc.run test_str.run test_sys.run test_rnd.run

%.run: %.bin
	if [ -f $(subst .bin,.expected, $<) ]; then    \
		./$< | diff $(subst .bin,.expected, $<) -; \
	else \
		./$<; \
	fi

%.bin: %.c libruntime.a
	gcc -I./include -O0 -g -fno-builtin -o $@ -static -nostdlib -L. $< -lruntime

libruntime.a: sys.o mem.o rnd.o str.o runtime.o
	ar cr $@ $^

.PRECIOUS: %.o %.bin %.a

%.o: %.asm
	as --gstabs -o $@ $< # -a

.PHONY: clean
clean:
	rm -f *.bin *.o *.a
