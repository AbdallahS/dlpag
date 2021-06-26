all: dlpag

.PHONY: clean dlpag

clean:
	rm -f *~ -r __pycache__
	dune clean

dlpag:
	dune build bin/Dlpag.exe
	cp _build/default/bin/Dlpag.exe dlpag
	chmod +w dlpag
#	ln -s _build/default/bin/Dlpag.exe dlpag

#lib:
#	ocamlbuild $(OPTIONS) -use-ocamlfind src/Dlpag.cma src/Dlpag.cmxa
#	ocamlbuild $(OPTIONS) src/Circuit.cma src/Circuit.cmxa
#	cp _build/src/Circuit.cmxa dlpag.cmxa
#	cp _build/src/Circuit.cma dlpag.cma


errorfile:
	menhir --list-errors lib/Parser.mly > lib/Parser.messages
