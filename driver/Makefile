CAMLC = ocamlc
JSOO = js_of_ocaml

ocamlopt.js: ocamlopt.byte
	$(JSOO) --pretty +dynlink.js +toplevel.js +weak.js ocamlopt.byte

ocamlopt.byte: optmain.ml
	$(CAMLC) -o ocamlopt.byte -I +compiler-libs ocamlcommon.cma ocamloptcomp.cma ocamlbytecomp.cma optmain.ml

clean:
	rm -rf *.cm* ocamlopt.byte ocamlopt.js

.PHONY: clean
