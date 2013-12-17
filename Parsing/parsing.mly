%token EOF CLASS STATIC 
%token OPENBRACKET CLOSEBRACKET
%token SEMICOLON
%token <string> UIDENT
%token <string> LIDENT

%start code

%type <Ast.minijava> code

%%
code:
  |CLASS classname=UIDENT EOF OPENBRACKET attribute_or_method CLOSEBRACKET {Class(classname)}
attribute_or_method:
  |attribute {}
  |class_method {}
attribute:
  |STATIC? UIDENT LIDENT (EQUAL expr)? SEMICOLON {}
class_method:
expr:
%%
