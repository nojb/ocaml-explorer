OCAMLC = ocamlc
JSOO = js_of_ocaml
JSOO_DIR = $(shell ocamlfind query js_of_ocaml)
JSOO_MKCMIS = jsoo_mkcmis
OCAML_SRCDIR = ocaml

all: stdlib.cmis.js jsdriver.js

open: all
	open index.html

INCLUDE = \
	$(addprefix -I $(OCAML_SRCDIR)/, utils parsing typing bytecomp middle_end asmcomp driver)

optmain.cmo optmain.cmi: optmain.ml
	$(OCAMLC) $(INCLUDE) -g -c $< -o $@

codemirror.cmo codemirror.cmi: codemirror.mli codemirror.ml
	$(OCAMLC) -I $(JSOO_DIR) -ppx $(JSOO_DIR)/ppx_js -g -c $^

jsdriver.cmo: jsdriver.ml optmain.cmi codemirror.cmi
	$(OCAMLC) -I $(JSOO_DIR) $(INCLUDE) -ppx $(JSOO_DIR)/ppx_js -g -c $< -o $@

COMPILERLIBS = \
	$(addprefix $(OCAML_SRCDIR)/compilerlibs/, ocamlcommon.cma ocamlbytecomp.cma ocamloptcomp.cma)

jsdriver.byte: codemirror.cmo optmain.cmo jsdriver.cmo
	$(OCAMLC) -I $(JSOO_DIR) $(INCLUDE) $(COMPILERLIBS) js_of_ocaml.cma -g $^ -o $@

jsdriver.js: jsdriver.byte
	$(JSOO) --extern-fs --pretty --source-map +weak.js +toplevel.js +dynlink.js $< -o $@

stdlib.cmis.js:
	$(JSOO_MKCMIS) -prefix /cmis $(OCAML_SRCDIR)/stdlib/stdlib.cma -o $@

test.byte: test.ml
	$(OCAMLC) -I $(JSOO_DIR) -ppx $(JSOO_DIR)/ppx_js js_of_ocaml.cma $< -g -o $@

test.js: test.byte
	$(JSOO) --pretty --source-map $< -o $@

clean:
	$(RM) *.cm* *.byte jsdriver.js *.annot *.map

.PHONY: reload clean stdlib.cmis.js
