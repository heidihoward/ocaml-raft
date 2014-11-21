ocamlfind ocamlc -package js_of_ocaml -package core_kernel_js -package js_of_ocaml.syntax \
	-syntax camlp4o -linkpkg -g -o ocamlraft.byte ocamlraft.ml

js_of_ocaml --pretty --noinline --disable genprim ocamlraft.byte