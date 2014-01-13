{
	open Ast
	open Parsing_mjava
	open Location
}

let space = [' ' '\t']
let end_line = ['\r' '\n']
let lowercase_letter = ['a'-'z'] 
let uppercase_letter = ['A'-'Z']
let word = ('_'|lowercase_letter|uppercase_letter)*
let lowercase_word = lowercase_letter word
let capitalized_word = uppercase_letter word 
let digit = ['0'-'9']
let number = digit+
let line_comment = "//" [^'\n']* '\n'
let multiline_comment = "/*"

rule read_string current_string = parse
	| '\n' { incr_line lexbuf; read_string current_string lexbuf }
	| '"' { current_string }
	| '\\' { raise (Compilation_Error (Unvalid_string Unvalid_escape, curr lexbuf)) }
	| eof { raise (Compilation_Error (Unvalid_string Open_string, curr lexbuf)) }
	| "\\\\" { read_string (current_string ^ "\\") lexbuf }
	| "\\\"" { read_string (current_string ^ "\"") lexbuf }
	| "\\n" { read_string (current_string ^ "\n") lexbuf }
	| _ as c { read_string (current_string ^ (Char.escaped c)) lexbuf }

and read_comment = parse
	| '\n' { incr_line lexbuf; read_comment lexbuf }
	| "*/" { token lexbuf }
	| eof { raise (Compilation_Error (Open_comment, (curr lexbuf))) }
	| _ { read_comment lexbuf }

and token = parse
	| space+ { token lexbuf }
	| end_line { incr_line lexbuf; token lexbuf }
	| number as current_nb { try 
														INTEGER (int_of_string current_nb)
													with Failure("int_of_string") ->
														raise (Compilation_Error (Number_Exception current_nb, curr lexbuf))
												 }
	| '"' { STRING (read_string "" lexbuf) } 
	| line_comment { incr_line lexbuf; token lexbuf }
	| multiline_comment { read_comment lexbuf }
  | "class" { CLASS }
  | "static" { STATIC }
  | "in" { IN }
  | "if" { IF }
  | "else" { ELSE }
  | "new" { NEW }
  | "instanceof" { INSTANCEOF }
  | "this" { THIS }
  | "null" { NULL }
  | "true" { TRUE }
  | "false" { FALSE }
  | lowercase_word as word { print_endline word; LIDENT word }
  | capitalized_word as word { print_endline word; UIDENT word }
  | '{' { print_endline "open"; OPENBRACKET }
  | '}' { print_endline "close"; CLOSEBRACKET }
  | '(' { print_endline "openPar"; OPENPAR }
  | ')' { print_endline "closePar"; CLOSEPAR }
  | ',' { COMMA }
  | ';' { print_endline "semicolon"; SEMICOLON }
  | '.' { DOT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '%' { MODULO }
  | "==" { EQUAL }
  | '=' { ASSIGN }
  | "!=" { DIFF }
  | '!' { NOT }
  | "<=" { INFEQ }
  | '<' { INF }
  | ">=" { SUPEQ }
  | '>' { SUP }
  | "&&" { AND }
  | "||" { OR }
  | eof { EOF } 
  | _ as c { raise (Compilation_Error (Unexpected_syntax c, curr lexbuf)) }

{
  let parsing =
    print_endline "parsing todo";
}
