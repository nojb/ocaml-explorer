CAMLC = ocamlc
JSOO = js_of_ocaml

ocamlopt.js: driver/ocamlopt
	$(JSOO) +dynlink.js +toplevel.js +weak.js driver/ocamlopt

driver/ocamlopt: driver/optmain.ml
	$(CAMLC) -o ocamlopt -I +compiler-libs ocamlcommon.cma ocamloptcomp.cma ocamlbytecomp.cma optmain.ml
