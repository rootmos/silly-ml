OCB_FLAGS := -use-ocamlfind -plugin-tag "package(ppx_driver.ocamlbuild)"
OCB := ocamlbuild $(OCB_FLAGS)

SRC=$(shell git ls-files)

.PHONY: repl
repl: repl.native
	rlwrap ./$<

.PHONY: test
test: parser_tests interpret_tests

%_tests: %_tests.native
	./$< inline-test-runner test # verbose -stop-on-error

.PHONY: snippet
snippet: snippet.native $(SRC)
	./$<

.PHONY: clean
clean:
	$(OCB) -clean

%.native: $(SRC)
	$(OCB) $@
