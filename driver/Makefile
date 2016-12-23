OCAMLC = ocamlc
JSOO = js_of_ocaml
JSOO_DIR = $(shell ocamlfind query js_of_ocaml)
JSOO_MKCMIS = jsoo_mkcmis
OCAML_SRCDIR = ../ocaml

all: stdlib.cmis.js test.js

open: all
	open index.html

INCLUDE = \
	$(addprefix -I $(OCAML_SRCDIR)/, utils parsing typing bytecomp middle_end asmcomp driver)

optmain.cmo: optmain.ml
	$(OCAMLC) $(INCLUDE) -c $< -o $@

test.cmo: test.ml optmain.cmo
	$(OCAMLC) -I $(JSOO_DIR) -ppx $(JSOO_DIR)/ppx_js -c $< -o $@

COMPILERLIBS = \
	$(addprefix $(OCAML_SRCDIR)/compilerlibs/, ocamlcommon.cma ocamlbytecomp.cma ocamloptcomp.cma)

jsdriver.byte: jsdriver.cmo
	$(OCAMLC) -I $(JSOO_DIR) $(INCLUDE) $(COMPILERLIBS) js_of_ocaml.cma optmain.cmo $< -o $@

jsdriver.js: jsdriver.byte
	$(JSOO) --extern-fs --pretty --source-map +weak.js +toplevel.js +dynlink.js $< -o $@

stdlib.cmis.js:
	$(JSOO_MKCMIS) -prefix /cmis $(OCAML_SRCDIR)/stdlib/stdlib.cma -o $@

clean:
	$(RM) *.cm* *.byte jsdriver.js *.annot *.map

.PHONY: reload clean stdlib.cmis.js
