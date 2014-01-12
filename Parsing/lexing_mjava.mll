{
	open Parsing_mjava
  exception Unexpected_syntax
}

let space = [' ' '\t']
let end_line = ['\r' '\n']
let lowercase_letter = ['a'-'z'] 
let uppercase_letter = ['A'-'Z']
let word = ('_'|lowercase_letter|uppercase_letter)*
let lowercase_word = lowercase_letter(word)
let capitalized_word = uppercase_letter(word) 
let digit = ['0'-'9']
let number = digit+
let line_comment = "//" [^'\n']* '\n'
let multiline_comment = "/*" _* "*/"

rule read_string current_string = parse
	| "\\\\" { read_string (current_string ^ "\\") lexbuf }
	| "\n" { read_string current_string lexbuf }
	| "\"" { read_string (current_string ^ "\"") lexbuf }
	| "\"" { current_string }
	| "\\" { raise (Ast.Error (Unvalid_string Unvalid_escape, (Location.curr lexbuf))) }
	| eof { raise (Ast.Error (Unvalid_string Open_string, (Location.curr lexbuf))) }
	| _ as c { read_string (current_string ^ (Char.escaped c)) lexbuf }

and token = parse
	| space+ { token lexbuf }
	| end_line { print_endline "new line"; Location.incr_line lexbuf; token lexbuf }
	| number as current_integer { INTEGER (int_of_string current_integer) }
	| "\"" { print_endline "Lecture chaine"; STRING (read_string "" lexbuf) } 
	| line_comment as word{ print_endline "Lecture commentaire"; print_endline word; Location.incr_line lexbuf; token lexbuf }
	| multiline_comment { print_endline "Lecture commentaire"; token lexbuf }
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
  | '(' { OPENPAR }
  | ')' { CLOSEPAR }
  | ',' { COMMA }
  | ';' { SEMICOLON }
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
  | _ { raise Unexpected_syntax }

{
  let parsing =
    print_endline "parsing todo";
}
