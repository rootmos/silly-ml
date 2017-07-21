libruntime.a: sys.o mem.o rnd.o str.o runtime.o
	ar cr $@ $^

.PHONY: test
test: test_malloc.run test_str.run test_sys.run test_rnd.run test_runtime.run

%.run: %.bin
	./run-test.sh $<

%.bin: %.c libruntime.a
	gcc -I./include -O0 -g -fno-builtin -o $@ -static -nostdlib -L. $< -lruntime

.PRECIOUS: %.o %.bin %.a

%.o: %.asm
	as -g -o $@ $< # -a

.PHONY: clean
clean:
	rm -f *.bin *.o *.a
