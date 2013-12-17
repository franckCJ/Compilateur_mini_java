{
  exception Unexpected_syntax

  type new_class =
  |Class of String
}

let letter = ['a'-'z'] 
let c_letter = ['A'-'Z']
let word = (letter)*
let c_word = c_letter(word) 
let digit = ['0'-'9']

rule token = parse
  |"Class "+c_word 

{
  let parsing =
    print_endline "parsing todo";
}
