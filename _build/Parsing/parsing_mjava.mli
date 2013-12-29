exception Error

type token = 
  | UIDENT of (string)
  | TRUE
  | THIS
  | SUPEQ
  | SUP
  | STRING of (string)
  | STATIC
  | SEMICOLON
  | PLUS
  | OR
  | OPENPAR
  | OPENBRACKET
  | NULL
  | NOT
  | NEW
  | MUL
  | MODULO
  | MINUS
  | LIDENT of (string)
  | INT of (int)
  | INSTANCEOF
  | INFEQ
  | INF
  | IN
  | IF
  | FALSE
  | EQUAL
  | EOF
  | ELSE
  | DOT
  | DIV
  | DIFF
  | COMMA
  | CLOSEPAR
  | CLOSEBRACKET
  | CLASS
  | ASSIGN
  | AND


val code: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.minijava)