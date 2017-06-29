OCB_FLAGS := -use-ocamlfind -plugin-tag "package(ppx_driver.ocamlbuild)"
OCB := ocamlbuild $(OCB_FLAGS)

SRC=$(shell git ls-files)

.PHONY: test
test: parser_tests.native $(SRC)
	./$< inline-test-runner test

.PHONY: clean
clean:
	$(OCB) -clean

%.native: $(SRC)
	$(OCB) $@
