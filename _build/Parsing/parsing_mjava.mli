exception Error

type token = 
  | UIDENT of (string)
  | TRUE
  | TIMES
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
  | MODULO
  | MINUS
  | LIDENT of (string)
  | INTEGER of (int)
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
  | BOOL of (bool)
  | ASSIGN
  | AND


val code: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.minijava)