OCB_FLAGS := -use-ocamlfind -plugin-tag "package(ppx_driver.ocamlbuild)"
OCB := ocamlbuild $(OCB_FLAGS)

SRC=$(shell ls src/*.ml src/*.mly src/*.mll)

.PHONY: repl
repl: repl.native
	rlwrap ./$<

test: parser_tests interpret_tests test-repl

test-repl: repl.native
	./repl.expect

%_tests: %_tests.native
	./$< inline-test-runner test -verbose -stop-on-error

.PHONY: snippet
snippet: snippet.native $(SRC)
	./$<

.PHONY: clean
clean:
	$(OCB) -clean

.PRECIOUS: %.native
%.native: $(SRC)
	$(OCB) $@

.PHONY: debug
debug: compiler.native
	./$< -L=runtime examples/simple01.silly-ml
	gdb a.out
