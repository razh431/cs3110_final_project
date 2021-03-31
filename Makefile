MODULES=board player resource state tile main author
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

# test:
# 	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

# check:
# 	@bash check.sh
	
# finalcheck:
# 	@bash check.sh final

# zip:
# 	zip adventure.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile	
	
# docs: docs-public docs-private
	
# docs-public: build
# 	mkdir -p _doc.public
# 	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
# 		-html -stars -d _doc.public $(MLIS)

# docs-private: build
# 	mkdir -p _doc.private
# 	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
# 		-html -stars -d _doc.private \
# 		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private adventure.zip
