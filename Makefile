
LIB=/home/dan/me/ocaml/lib

.PHONY:  entries clean minml string sanitize test ruletest metl all

entries:
	ocamlbuild -cflags -I,+camlp4 -pp camlp4rf entries.cmo
	cp _build/entries.cmo $(LIB)/

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

minpp:
	camlp4rf ../lib/PaMetlString.cmo Minml/rules.ml

test:
	ocamlbuild -libs dynlink,camlp4lib,MetlStringAux \
	-cflags -I,+camlp4,-I,$(LIB) \
	-lflags -I,+camlp4,-I,$(LIB) \
        -pp "camlp4of $(LIB)/PaMetlString.cmo" \
	metltest.byte
#	ocamlc -I +camlp4 -I $(LIB) -pp "camlp4of $(LIB)/PaMetlString.cmo" \
#       dynlink.cma camlp4lib.cma MetlStringAux.cma -o metltest.b metltest.ml

ruletest:
	minml.byte Minml/ruletest.ml

mancode:
	ocamlbuild -pp "camlp4of ../../lib/PaMetlString.cmo" \
	-libs dynlink,camlp4lib,MetlStringAux \
	-cflags -I,+camlp4,-I,$(LIB) \
	-lflags -I,+camlp4,-I,$(LIB) \
	 mancode.byte

metl:  string test
	./metltest.byte

all:  string test minml ruletest
