{
  exception Unexpected_syntax
}

let lowercase_letter = ['a'-'z'] 
let uppercase_letter = ['A'-'Z']
let lowercase_word = (lowercase_letter)*
let capitalized_word = uppercase_letter(lowercase_word) 
let digit = ['0'-'9']

rule token = parse
  |"class" {CLASS}
  |lowercase_word as word {LIDENT word}
  |capitalized_word as word {UIDENT word}
  |eof {EOF} 
  |_ {raise new Unexpected_syntax}

{
  let parsing =
    print_endline "parsing todo";
}
