RENDERER=pervasives_js list_js char_js string_js array_js renderer
PARSER=tokenizer grammar parser ast_converter
OPTIMIZER=deduplicate_ident curry_optimizer unused_binding_optimizer \
				  tail_rec_optimizer call_squasher optimizer
TYPES=parser/ast.mli parser/token.mli parser/lr_action.mli parser/parse_tree.mli
MISC=file_helper
OBJECTS=$(MISC:=.cmo) $(RENDERER:=.cmo) $(PARSER:=.cmo) $(OPTIMIZER:=.cmo)
MLS=$(MISC:=.ml) $(RENDERER:%=renderer/%.ml) $(PARSER:%=parser/%.ml) \
		$(OPTIMIZER:%=optimizer/%.ml)
MLIS=$(MODULES:=.mli)  $(TYPES)
TEST=test.byte
E2ETEST=end_to_end_test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -I "renderer" -I "parser" -I "optimizer"
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

parser/token.mli: grammar.json
	$(OCAMLBUILD) $(CONVERTER) && ./$(CONVERTER)
parser/tokenizer.ml parser/tokenizer.mli parser/grammar.ml: parser/token.mli

grammar: parser/token.mli parser/tokenizer.ml parser/tokenizer.mli \
				 parser/grammar.ml

zip: clean
	zip camljs_src.zip *.ml *.mli end_to_end_tests/* renderer/* *.json _tags \
	Makefile README.md

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build  -I "_build/renderer" -I "_build/parser" \
		-I "_build/optimizer" -package $(PKGS) -html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -I "_build/renderer" -I "_build/parser" \
	 	-I "_build/optimizer" -package $(PKGS) -html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS) converter.ml

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private
	rm -rf parser/grammar.ml parser/tokenizer.ml parser/tokenizer.mli parser/token.mli
	rm -rf temp.js temp.ml
	rm -rf camljs_src.zip

.PHONY: test, temp-debug, bin, grammar, docs, docs-public, docs-private, \
				build, e2e-test, zip
