
LIB=/home/dan/me/ocaml/lib

.PHONY:  clean test minml string sanitize

string:
	ocamlbuild -cflags -I,+camlp4 -Is Metl,MetlString,MetlString/Parser MetlString/PaMetlString.cmo
	ocamlbuild                    -Is      MetlString,MetlString/Aux    MetlString/MetlString.cma
	cp _build/MetlString/*MetlString*.cm? $(LIB)

test:
	ocamlfind ocamlc -o test -package oUnit -linkpkg -g foo.ml test.ml

clean:
	rm -rf _build

sanitize:
	_build/sanitize.sh

minml:
#	ocamlbuild -I _build/MetlString -cflags -I,+camlp4 Minml/minml.byte
	ocamlbuild -libs dynlink,camlp4lib,MetlString \
	-cflags -I,+camlp4,-I,$(LIB) \
	-lflags -I,+camlp4,-I,$(LIB) \
	 Minml/minml.byte

ruletest:
	ocamlbuild -cflags -I,+camlp4,-I,$(LIB) -lflags -I,+camlp4,-I,$(LIB) Minml/ruletest.cmo

