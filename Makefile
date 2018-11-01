JS_MODULES=pervasives_js list_js char_js string_js
MODULES=tokenizer grammar parser ast_converter renderer file_helper
OBJECTS=$(MODULES:=.cmo) $(JS_MODULES:=.cmo)
MLS=$(MODULES:=.ml) $(JS_MODULES:%=js_modules/%.ml)
MLIS=$(MODULES:=.mli) ast.mli token.mli $(JS_MODULES:%=js_modules/%.mli)
TEST=test.byte
E2ETEST=end_to_end_test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -I "js_modules"
PKGS=oUnit,str,yojson
EXEC=camljs
CONVERTER=converter.byte

default: bin

build: grammar
	$(OCAMLBUILD) $(OBJECTS)

bin: build
	$(OCAMLBUILD) $(MAIN) && mv $(MAIN) $(EXEC)

test: build
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

e2e-test: bin
	$(OCAMLBUILD) -tag 'debug' $(E2ETEST) && ./$(E2ETEST)

temp-debug: build
	$(OCAMLBUILD) -tag 'debug' temp.byte && ocamldebug ./temp.byte

docs: docs-public docs-private

grammar:
	$(OCAMLBUILD) $(CONVERTER) && ./$(CONVERTER) grammar.json

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -I "_build/js_modules" -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -I "_build/js_modules" -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS) converter.ml

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private
	rm -rf grammar.ml tokenizer.ml tokenizer.mli token.mli temp.js temp.ml

.PHONY: test, test-debug, bin, grammar
