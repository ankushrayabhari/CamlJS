MODULES=tokenizer grammar parser renderer main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,str
EXEC=camljs

default: bin

build:
	$(OCAMLBUILD) $(OBJECTS)

bin: build
	$(OCAMLBUILD) $(MAIN) && mv $(MAIN) $(EXEC)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

test-debug:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ocamldebug ./$(TEST)
.PHONY: test, test-debug, bin

docs: docs-public docs-private

grammar:
	node converter.js > grammar.ml

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
