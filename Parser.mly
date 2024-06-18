%{
open Printf
open Backend
let initsheet = ref [];;
let filereader name= try
	let inputfile = open_in name in
	while true do
		let line = (input_line inputfile) in (initsheet := (insertrowinsheet !initsheet (makecellrow (String.split_on_char ',' line) [])))
	done
with End_of_file -> Printf.printf "sheet initialised\n";; 
(*let cell_list = (c_list 10 [] Empty);;	     (*initialising cell with some values*)					
let initsheet = (slist 10 [] cell_list);;	(*initialisaing sheet with a some cells*)
let initsheet = (modifycellofsheet 0 0 initsheet (Float 1.0));;
let initsheet = (modifycellofsheet 1 0 initsheet (Float 2.0));;
let initsheet = (modifycellofsheet 2 0 initsheet (Float 3.0));;
let initsheet = (modifycellofsheet 0 1 initsheet (Float 10.0));;
let initsheet = (modifycellofsheet 1 1 initsheet (Float (20.0)));;
let initsheet = (modifycellofsheet 2 1 initsheet (Float 30.0));;
let initsheet = (modifycellofsheet 0 2 initsheet (Float 100.0));;
let initsheet = (modifycellofsheet 1 2 initsheet (Float 200.0));;
let initsheet = (modifycellofsheet 2 2 initsheet (Float 300.0));;*)
filereader Sys.argv.(1);;
(printsheet !initsheet);;
%}

%token <float> FL
%token LEFTPAR RIGHTPAR LBRACKET RBRACKET COMMA COLON COUNT ROWCOUNT COLCOUNT SUM ROWSUM COLSUM AVG ROWAVG COLAVG MIN ROWMIN COLMIN MAX 
%token ROWMAX COLMAX ADD SUB MULT DIV ASSIGNMENT SEMICOLON EOF NL
%token <int> INTVAL

%start input			
%type <unit> input
%type <range> r
%type <index> i
%type <sheet> formula

%%
input : {}
| input line {printsheet !initsheet}
;

line : NL {}
| formula NL {initsheet := $1}
;


formula : i ASSIGNMENT COUNT r SEMICOLON  {full_count !initsheet $4 $1}
| i ASSIGNMENT ROWCOUNT r SEMICOLON	{row_count !initsheet $4 $1}	
| i ASSIGNMENT COLCOUNT r SEMICOLON	{col_count !initsheet $4 $1}
| i ASSIGNMENT SUM r SEMICOLON		{full_sum !initsheet $4 $1}
| i ASSIGNMENT ROWSUM r SEMICOLON		{row_sum !initsheet $4 $1}
| i ASSIGNMENT COLSUM r SEMICOLON		{col_sum !initsheet $4 $1}
| i ASSIGNMENT AVG r SEMICOLON		{full_avg !initsheet $4 $1}
| i ASSIGNMENT ROWAVG r SEMICOLON		{row_avg !initsheet $4 $1}
| i ASSIGNMENT COLAVG r SEMICOLON		{col_avg !initsheet $4 $1}
| i ASSIGNMENT MIN r SEMICOLON		{full_min !initsheet $4 $1}
| i ASSIGNMENT ROWMIN r SEMICOLON		{row_min !initsheet $4 $1}
| i ASSIGNMENT COLMIN r SEMICOLON		{col_min !initsheet $4 $1}
| i ASSIGNMENT MAX r SEMICOLON		{full_max !initsheet $4 $1}
| i ASSIGNMENT ROWMAX r SEMICOLON		{row_max !initsheet $4 $1}
| i ASSIGNMENT COLMAX r SEMICOLON		{col_max !initsheet $4 $1}
| i ASSIGNMENT ADD r r SEMICOLON		{add_range !initsheet $5 $4 $1}
| i ASSIGNMENT SUB r r SEMICOLON		{subt_range !initsheet $5 $4 $1}
| i ASSIGNMENT MULT r r SEMICOLON		{mult_range !initsheet $5 $4 $1}
| i ASSIGNMENT DIV r r SEMICOLON		{div_range !initsheet $5 $4 $1}
| i ASSIGNMENT ADD FL r SEMICOLON		{add_const !initsheet $5 $4 $1}
| i ASSIGNMENT SUB FL r SEMICOLON		{subt_const !initsheet $5 $4 $1}
| i ASSIGNMENT MULT FL r SEMICOLON	{mult_const !initsheet $5 $4 $1}
| i ASSIGNMENT DIV FL r SEMICOLON		{div_const !initsheet $5 $4 $1}
| i ASSIGNMENT ADD r FL SEMICOLON		{add_const !initsheet $4 $5 $1}
| i ASSIGNMENT SUB r FL SEMICOLON		{subt_const !initsheet $4 $5 $1}
| i ASSIGNMENT MULT r FL SEMICOLON	{mult_const !initsheet $4 $5 $1}
| i ASSIGNMENT DIV r FL SEMICOLON		{div_const !initsheet $4 $5 $1}
| i ASSIGNMENT ADD INTVAL r SEMICOLON		{add_const !initsheet $5 (float_of_int $4) $1}
| i ASSIGNMENT SUB INTVAL r SEMICOLON		{subt_const !initsheet $5 (float_of_int $4) $1}
| i ASSIGNMENT MULT INTVAL r SEMICOLON	{mult_const !initsheet $5 (float_of_int $4) $1}
| i ASSIGNMENT DIV INTVAL r SEMICOLON		{div_const !initsheet $5 (float_of_int $4) $1}
| i ASSIGNMENT ADD r INTVAL SEMICOLON		{add_const !initsheet $4 (float_of_int $5) $1}
| i ASSIGNMENT SUB r INTVAL SEMICOLON		{subt_const !initsheet $4 (float_of_int $5) $1}
| i ASSIGNMENT MULT r INTVAL SEMICOLON	{mult_const !initsheet $4 (float_of_int $5) $1}
| i ASSIGNMENT DIV r INTVAL SEMICOLON		{div_const !initsheet $4 (float_of_int $5) $1}
| i ASSIGNMENT ADD i r SEMICOLON		{add_index !initsheet $5 $4 $1}
| i ASSIGNMENT SUB i r SEMICOLON		{subt_index !initsheet $5 $4 $1}
| i ASSIGNMENT MULT i r SEMICOLON		{mult_index !initsheet $5 $4 $1}
| i ASSIGNMENT DIV i r SEMICOLON		{div_index !initsheet $5 $4 $1}
| i ASSIGNMENT ADD r i SEMICOLON		{add_index !initsheet $4 $5 $1}
| i ASSIGNMENT SUB r i SEMICOLON		{subt_index !initsheet $4 $5 $1}
| i ASSIGNMENT MULT r i SEMICOLON		{mult_index !initsheet $4 $5 $1}
| i ASSIGNMENT DIV r i SEMICOLON		{div_index !initsheet $4 $5 $1}
;

r : LEFTPAR i COLON i RIGHTPAR	{{ind1=$2;ind2=$4}}
;

i : LBRACKET INTVAL COMMA INTVAL RBRACKET {{integer1=$2;integer2=$4}}
;

%%
(*line production rule updates initsheet once the formula read is successful*)
(*formular calls appropriate functions based on command*)
(*r production rule returns data of range type*)
(*i production rule returns data of index type*)


