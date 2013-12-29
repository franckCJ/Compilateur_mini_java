{
	open Parsing_mjava
  exception Unexpected_syntax
}

let space = [' ' '\t' '\n' '\r']
let lowercase_letter = ['a'-'z'] 
let uppercase_letter = ['A'-'Z']
let lowercase_word = lowercase_letter('_'|lowercase_letter)*
let capitalized_word = uppercase_letter(lowercase_word) 
let digit = ['0'-'9']

rule token = parse
	| space+ { token lexbuf }
  | "class" { CLASS }
  | "static" { STATIC }
  | lowercase_word as word { LIDENT word }
  | capitalized_word as word { UIDENT word }
  | '{' { OPENBRACKET }
  | '}' { CLOSEBRACKET }
  | '(' { OPENPAR }
  | ')' { CLOSEPAR }
  | ';' { SEMICOLON }
  | eof { EOF } 
  | _ { raise Unexpected_syntax }

{
  let parsing =
    print_endline "parsing todo";
}
