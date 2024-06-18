{
exception EOFException
open Parser  (*Assumes the parser file is Parser.mly*)
open Printf
}

(*removed rans and index as the tokens, because it wasn't taking whitespaces in lex into account,i.e. it expected a more strict format*)
(* forgot to define integer data type for indexes, added comma and colon*)
(*also changed a name of few tokens just for convenience*)

rule sendtoken = parse
        | ['-''+']?['1'-'9']['0'-'9']*(['.']['0'-'9']*['1'-'9']+) as floatval       {FL(float_of_string floatval)}
	| ['1'-'9']['0'-'9']* as integerval		{INTVAL(int_of_string integerval)}
	| "0" 					{INTVAL(0)}        
	| "("                                   {LEFTPAR}
        | ")"                                   {RIGHTPAR}
        | "["                                   {LBRACKET}
        | "]"                                   {RBRACKET}
        | "ADD"         {ADD}
        | "SUBT"         {SUB}
        | "MULT"        {MULT}
        | "DIV"         {DIV}
        | "COUNT"       {COUNT}
        | "ROWCOUNT"    {ROWCOUNT}
        | "COLCOUNT"    {COLCOUNT}
        | "SUM"         {SUM}
        | "ROWSUM"      {ROWSUM}
        | "COLSUM"      {COLSUM}
        | "AVG"         {AVG}
        | "ROWAVG"      {ROWAVG}
        | "COLAVG"      {COLAVG}
        | "MIN"         {MIN}
        | "ROWMIN"      {ROWMIN}
        | "COLMIN"      {COLMIN}
        | "MAX"         {MAX}
        | "ROWMAX"      {ROWMAX}
        | "COLMAX"      {COLMAX}
	| ","		{COMMA}
        | ":="          {ASSIGNMENT}
        | ";"           {SEMICOLON}
	| ":"		{COLON}
        | '\n'            {NL}
        | [' ''\t']         {sendtoken lexbuf}
        | eof           {raise EOFException}

