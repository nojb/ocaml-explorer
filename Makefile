ocaml: ocaml.patch
	cd ocaml
	patch -p1 < ../ocaml.patch
	configure && $(MAKE) world.opt

driver: ocaml
	$(MAKE) -C driver stdlib.cmis.js
	$(MAKE) -C driver test.js

.PHONY: driver
