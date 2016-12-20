CAMLC = ocamlc
JSOO = js_of_ocaml

all: test.js
	open index.html

ocamlopt.js: ocamlopt.byte
	$(JSOO) --pretty +dynlink.js +toplevel.js +weak.js $<

ocamlopt.byte: optmain.ml
	$(CAMLC) -o $@ -I +compiler-libs ocamlcommon.cma ocamloptcomp.cma ocamlbytecomp.cma $<

stdlib.cmis.js:
	jsoo_mkcmis -verbose stdlib -o stdlib.cmis.js

test.byte: optmain.ml test.ml
	ocamlfind ocamlc -annot -package js_of_ocaml.ppx -linkpkg \
	-I +compiler-libs ocamlcommon.cma ocamloptcomp.cma ocamlbytecomp.cma \
	optmain.ml test.ml -o test.byte

test.js: test.byte
	js_of_ocaml --debug-info --pretty --extern-fs +dynlink.js +toplevel.js +weak.js test.byte

clean:
	rm -rf *.cm* *.byte *.js *.annot

.PHONY: all clean stdlib.cmis.js
