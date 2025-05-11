all: dlpag

.PHONY: clean dlpag

clean:
	rm -f *~ -r __pycache__
	dune clean

dlpag:
	dune build bin/QcirPrenexer.exe
	dune build bin/Main.exe

install:
	dune build @install
	dune install

test:
	dune runtest

uninstall:
	dune uninstall


errorfile:
	menhir --list-errors lib/Parser.mly > lib/Parser.messages

errorfile_qcir:
	menhir --list-errors lib/ParserQCIR.mly > lib/ParserQCIR.messages
