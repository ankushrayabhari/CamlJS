MODULES=tokenizer grammar parser ast renderer main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,str
EXEC=camljs
CONVERTER=converter.byte

default: bin

build: grammar
	$(OCAMLBUILD) $(OBJECTS)

bin: build
	$(OCAMLBUILD) $(MAIN) && mv $(MAIN) $(EXEC)

test: grammar
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

test-debug:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ocamldebug ./$(TEST)

docs: docs-public docs-private

grammar:
	$(OCAMLBUILD) $(CONVERTER) && ./$(CONVERTER) grammar.json

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private
	rm -rf grammar.ml tokenizer.ml tokenizer.mli

.PHONY: test, test-debug, bin, grammar
