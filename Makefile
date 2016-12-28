OCAMLC = ocamlc
OCAMLFLAGS = -annot -safe-string -bin-annot -g
JSOO = js_of_ocaml
JSOO_DIR = $(shell ocamlfind query js_of_ocaml)
JSOO_MKCMIS = jsoo_mkcmis
OCAML_SRCDIR = ocaml

all: stdlib.cmis.js jsdriver.js

open: all
	open index.html

INCLUDE = \
	$(addprefix -I $(OCAML_SRCDIR)/, utils parsing typing bytecomp middle_end asmcomp driver)

jsooOpt.cmo jsooOpt.cmi: jsooOpt.ml
	$(OCAMLC) $(INCLUDE) $(OCAMLFLAGS) -c $< -o $@

codemirror.cmo codemirror.cmi: codemirror.mli codemirror.ml
	$(OCAMLC) -I $(JSOO_DIR) -ppx $(JSOO_DIR)/ppx_js $(OCAMLFLAGS) -c $^

jsdriver.cmo: jsdriver.ml jsooOpt.cmi codemirror.cmi
	$(OCAMLC) -I $(JSOO_DIR) $(INCLUDE) -ppx $(JSOO_DIR)/ppx_js $(OCAMLFLAGS) -c $< -o $@

COMPILERLIBS = \
	$(addprefix $(OCAML_SRCDIR)/compilerlibs/, ocamlcommon.cma ocamlbytecomp.cma ocamloptcomp.cma)

jsdriver.byte: codemirror.cmo jsooOpt.cmo jsdriver.cmo
	$(OCAMLC) -I $(JSOO_DIR) $(INCLUDE) $(COMPILERLIBS) js_of_ocaml.cma $(OCAMLFLAGS) $^ -o $@

jsdriver.js: jsdriver.byte
	$(JSOO) --extern-fs --pretty --source-map +weak.js +toplevel.js +dynlink.js $< -o $@

stdlib.cmis.js:
	$(JSOO_MKCMIS) -prefix /cmis $(OCAML_SRCDIR)/stdlib/stdlib.cma -o $@

PUBLISH = \
	index.html jsdriver.js jsdriver.map stdlib.cmis.js \
	thirdparty/codemirror.css thirdparty/codemirror.js thirdparty/gas.js thirdparty/mllike.js

publish: all
	git checkout --orphan tmp
	git reset
	git add -f $(PUBLISH)
	git commit -m "Publish"
	git push -u origin tmp:gh-pages --force
	git checkout -f master
	git branch -D tmp

clean:
	$(RM) *.cm* *.byte jsdriver.js stdlib.cmis.js *.annot *.map

.PHONY: clean stdlib.cmis.js
