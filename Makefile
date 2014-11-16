all:
	ocaml setup.ml -build
test: 
	corebuild -Is lib,test,spl test/test_splaytree.byte test/test_spl.byte test/test_spl_wrapper.byte
