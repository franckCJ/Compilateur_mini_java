%{
	open Ast
%}

%token EOF CLASS STATIC IN IF ELSE NEW INSTANCEOF THIS NULL TRUE FALSE (*mots clés*)
%token OPENBRACKET CLOSEBRACKET OPENPAR CLOSEPAR (**)
%token COMMA SEMICOLON DOT ASSIGN (*ponctuation*)
%token PLUS MINUS TIMES DIV MODULO NOT EQUAL DIFF INF INFEQ SUP SUPEQ AND OR (*opérateurs*)
%token <string> UIDENT
%token <string> LIDENT
%token <string> STRING
%token <int> INTEGER

%start code
%type <Ast.minijava> code

%left SEMICOLON
%nonassoc ASSIGN
%left OR
%left AND
%left EQUAL DIFF
%left INF INFEQ SUP SUPEQ
%left PLUS MINUS
%left TIMES DIV MODULO
%right UMINUS NOT

%%
code:
	| class_or_expr* EOF { File($1) } class_or_expr: | program_class { $1 } | expr { Expression $1 }
program_class:
  | CLASS UIDENT OPENBRACKET attribute_or_method* CLOSEBRACKET EOF { Class($2,$4) }
attribute_or_method:
  | attribute { $1 }
  | class_method { $1 }
attribute:
  | static UIDENT LIDENT instanciation SEMICOLON { Attribute ($3,$2,$1,$4) }
instanciation:
	| { None }
	| ASSIGN expr { $2 }
class_method:
	| static UIDENT LIDENT OPENPAR params CLOSEPAR OPENBRACKET expr CLOSEBRACKET { Method($3,$2,$1,$5,$8) }
params:
	| { [] }
	| UIDENT LIDENT { [Param ($2,$1)] }
	| UIDENT LIDENT COMMA params { (Param ($2,$1))::$4 }
static:
	| { false }
	| STATIC  { true }
expr:
	| OPENPAR expr CLOSEPAR { $2 }
	| LIDENT { Variable $1 }
	| INTEGER { Integer $1 }
	| STRING { String $1 }
	| NULL { None }
	| TRUE { Boolean true }
	| FALSE { Boolean false }
	| THIS { Self }
	| unop expr { Unop ($1,$2) }
	| expr binop expr { Binop ($2,$1,$3) }
	| LIDENT ASSIGN expr { Assignment ($1,$3) }
	| UIDENT LIDENT ASSIGN expr IN expr { Locassign ($2,$1,$4,$6) }
	| IF OPENPAR expr CLOSEPAR OPENBRACKET expr CLOSEBRACKET ELSE OPENBRACKET expr CLOSEBRACKET { Condition ($3,$6,$10) }
	| expr DOT LIDENT OPENPAR args CLOSEPAR { Method_call ($1,$3,$5) }
	| NEW UIDENT { Object $2 }
	| OPENPAR UIDENT CLOSEPAR expr { Cast ($4,$2) }
	| expr INSTANCEOF UIDENT { Instance ($1,$3) }
args:
	| { [] }
	| expr { [$1] }
	| expr COMMA args { $1::$3 }
unop:
	| MINUS %prec UMINUS { Uopposite }
	| NOT { Unot }
binop:
	| SEMICOLON { Bdel } (*Opérateur binaire de séparation de 2 expressions*)
	| INF { Binf }
	| INFEQ { Binfeq }
	| SUP { Bsup }
	| SUPEQ { Bsupeq }
	| DIFF { Bdiff }
	| EQUAL { Bequal }
	| PLUS { Badd }
	| MINUS { Bsub }
	| TIMES { Bmul }
	| DIV { Bdiv }
	| MODULO { Bmod }
	| AND { Band }
	| OR { Bor }
%%
