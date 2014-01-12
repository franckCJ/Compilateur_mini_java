%{
%}

%token EOF CLASS STATIC IN IF ELSE NEW INSTANCEOF THIS NULL TRUE FALSE (*mots clés*)
%token OPENBRACKET CLOSEBRACKET OPENPAR CLOSEPAR (**)
%token COMMA SEMICOLON DOT ASSIGN (*ponctuation*)
%token PLUS MINUS TIMES DIV MODULO NOT EQUAL DIFF INF INFEQ SUP SUPEQ AND OR(*opérateurs*)
%token <string> UIDENT
%token <string> LIDENT
%token <string> STRING
%token <int> INTEGER
%token <bool> BOOL

%start code

%type <Ast.minijava> code

%%
code:
	| e=class_or_expr* EOF { Ast.File(e) }
class_or_expr:
	| current_class=program_class { current_class }
	(*| current_expr=expr { current_expr }*)
program_class:
  | CLASS classname=UIDENT OPENBRACKET elements=attribute_or_method* CLOSEBRACKET EOF { Ast.Class(classname, elements) }
attribute_or_method:
  | attr=attribute { attr }
  | meth=class_method { meth }
attribute:
  | STATIC? UIDENT name=LIDENT instanciation? SEMICOLON { Ast.Attribute(name) }
instanciation:
	| ASSIGN expr {}
class_method:
	| STATIC? UIDENT name=LIDENT OPENPAR params? CLOSEPAR OPENBRACKET expr? CLOSEBRACKET { Ast.Method(name, []) }
params:
	| UIDENT LIDENT sup_params* {}
sup_params:
	| COMMA UIDENT LIDENT {}
expr:
	| OPENPAR expr CLOSEPAR {}
	| LIDENT {}
	| INTEGER {}
	| STRING {}
	| NULL {}
	| TRUE {}
	| FALSE {}
	| THIS {}
	| unop expr {}
	| expr binop expr {}
	| LIDENT ASSIGN expr {}
	| UIDENT LIDENT ASSIGN expr IN expr {}
	| IF OPENPAR expr CLOSEPAR OPENBRACKET expr CLOSEBRACKET ELSE OPENBRACKET expr CLOSEBRACKET {}
	| expr DOT LIDENT OPENPAR args? CLOSEPAR {}
	| NEW UIDENT {}
	| OPENPAR UIDENT CLOSEPAR expr {}
	| expr INSTANCEOF UIDENT {}
args:
	| expr sup_args* {}
sup_args:
	| COMMA expr {}
unop:
	| MINUS {}
	| NOT {}
binop:
	| SEMICOLON {} (*Opérateur binaire de séparation de 2 expressions*)
	| INF {}
	| INFEQ {}
	| SUP {}
	| SUPEQ {}
	| DIFF {}
	| EQUAL {}
	| PLUS {}
	| MINUS {}
	| TIMES {}
	| DIV {}
	(*| MODULO {}
	| AND {}
	| OR {}*)
%%
