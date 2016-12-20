CAMLC = ocamlc
JSOO = js_of_ocaml
JSOO_DIR = $(shell ocamlfind query js_of_ocaml)

reload: test.js
	open index.html

INCLUDES = \
	-I ../ocaml/utils -I ../ocaml/parsing \
	-I ../ocaml/typing -I ../ocaml/bytecomp \
	-I ../ocaml/middle_end \
	-I ../ocaml/asmcomp -I ../ocaml/driver \
	-I ../ocaml/compilerlibs -I ../ocaml/stdlib

stdlib.cmis.js:
	jsoo_mkcmis -verbose stdlib -o stdlib.cmis.js

test.byte: optmain.ml test.ml
	$(CAMLC) -verbose -nostdlib -annot -I $(JSOO_DIR) \
		$(INCLUDES) stdlib.cma ocamlcommon.cma ocamloptcomp.cma ocamlbytecomp.cma \
		-ppx $(JSOO_DIR)/ppx_js js_of_ocaml.cma optmain.ml test.ml -o test.byte

test.js: test.byte
	js_of_ocaml --debug-info --pretty --extern-fs +dynlink.js +toplevel.js +weak.js test.byte

clean:
	$(RM) *.cm* *.byte *.js *.annot

.PHONY: reload clean stdlib.cmis.js
