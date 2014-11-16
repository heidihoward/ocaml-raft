.PHONY: all clean build
all: build 

setup.bin: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	./setup.bin -configure

build: setup.data setup.bin
	./setup.bin -build -classic-display

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log setup.bin