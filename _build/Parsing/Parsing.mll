{
  exception Unexpected_syntax

  type new_class =
  |Class of String
}

let lowercase_letter = ['a'-'z'] 
let uppercase_letter = ['A'-'Z']
let lowercase_word = (lowercase_letter)*
let uppercase_word = uppercase_letter(lowercase_word) 
let digit = ['0'-'9']

rule token = parse
  |"class "+uppercase_word {Class(uppercase_word)}
  |_ {raise new Unexpected_syntax}

{
  let parsing =
    print_endline "parsing todo";
}
