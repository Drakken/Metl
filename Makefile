
LIB=/home/dan/me/ocaml/lib

.PHONY:  clean minml string sanitize test all

string:
	ocamlbuild -cflags -I,+camlp4 -Is Metl,MetlString MetlString/PaMetlString.cmo
	ocamlbuild                    -I       MetlString MetlString/MetlStringAux.cma
	cp _build/MetlString/*MetlString*.cm? $(LIB)

clean:
	rm -rf _build

sanitize:
	_build/sanitize.sh

minml:
	ocamlbuild -libs dynlink,camlp4lib,MetlStringAux \
	-cflags -I,+camlp4,-I,$(LIB) \
	-lflags -I,+camlp4,-I,$(LIB) \
	 Minml/minml.byte

test:
	minml.byte Minml/ruletest.ml

all: string minml test
