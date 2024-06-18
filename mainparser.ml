open Lexer
open Parser
open Printf
let main()=
	try
	let commandfile = open_in Sys.argv.(2) in
        let lexbuf = Lexing.from_channel commandfile
	in while(true) do 
		Parser.input Lexer.sendtoken lexbuf;
		Printf.printf "Reading\n";
	done
	with Lexer.EOFException -> exit(0);;
main();;
