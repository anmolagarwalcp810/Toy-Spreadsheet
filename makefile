all: Lexer.mll Parser.mly Backend.ml mainparser.ml
	ocamlc -c Backend.ml
	ocamlyacc Parser.mly
	ocamlc -c Parser.mli
	ocamllex Lexer.mll
	ocamlc -c Lexer.ml
	ocamlc -c Parser.ml
	ocamlc -c mainparser.ml
	ocamlc -o spreadsheet Backend.cmo Lexer.cmo Parser.cmo mainparser.cmo

clean:
	rm *.mli Lexer.ml Parser.ml *.cmi *.cmo spreadsheet
