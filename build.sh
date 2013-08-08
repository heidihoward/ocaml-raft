rm *.log
rm *.{byte,native}
ocamlbuild -use-ocamlfind msg.byte
ocamlbuild -use-ocamlfind node.byte
ocamlbuild -use-ocamlfind sim.byte

ocamlbuild -use-ocamlfind msg.native
ocamlbuild -use-ocamlfind node.native
ocamlbuild -use-ocamlfind sim.native

