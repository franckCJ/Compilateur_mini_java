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
%left OR
%left AND
%left EQUAL DIFF
%left INF INFEQ SUP SUPEQ
%left PLUS MINUS
%left TIMES DIV MODULO
%right UMINUS NOT

%%
code:
	| e=class_or_expr* EOF { File(e) }
class_or_expr:
	| current_class=program_class { current_class }
	| current_expr=expr { Expression current_expr }
program_class:
  | CLASS classname=UIDENT OPENBRACKET elements=attribute_or_method* CLOSEBRACKET EOF { Class(classname, elements) }
attribute_or_method:
  | attr=attribute { attr }
  | meth=class_method { meth }
attribute:
  | stat=static val_type=UIDENT name=LIDENT value=instanciation SEMICOLON { Attribute (name,val_type,stat,value) }
instanciation:
	| { None }
	| ASSIGN value=expr { print_endline "test"; value }
class_method:
	| stat=static val_type=UIDENT name=LIDENT OPENPAR param_list=params CLOSEPAR OPENBRACKET body=expr CLOSEBRACKET { Method(name,val_type,stat,param_list,body) }
params:
	| { [] }
	| val_type=UIDENT name=LIDENT { [Param (name,val_type)] }
	| val_type=UIDENT name=LIDENT COMMA param_list=params { (Param (name,val_type))::param_list }
static:
	| { false }
	| STATIC  { true }
expr:
	| OPENPAR value=expr CLOSEPAR { value }
	| name=LIDENT { Variable name }
	| value=INTEGER { Integer value }
	| value=STRING { String value }
	| NULL { None }
	| TRUE { Boolean true }
	| FALSE { Boolean false }
	| THIS { Self }
	| op=unop value=expr { Unop (op,value) }
	| value1=expr op=binop value2=expr { Binop (op,value1,value2) }
	| name=LIDENT ASSIGN value=expr { Assignment (name,value) }
	| val_type=UIDENT name=LIDENT ASSIGN value=expr IN target=expr { Locassign (name,val_type,value,target) }
	| IF OPENPAR expr_if=expr CLOSEPAR OPENBRACKET expr_then=expr CLOSEBRACKET ELSE OPENBRACKET expr_else=expr CLOSEBRACKET { Condition (expr_if,expr_then, expr_else) }
	| container=expr DOT method_name=LIDENT OPENPAR arg_list=args CLOSEPAR { Method_call (container,method_name,arg_list) }
	| NEW val_type=UIDENT { Object val_type }
	| OPENPAR val_type=UIDENT CLOSEPAR value=expr { Cast (value,val_type) }
	| value=expr INSTANCEOF val_type=UIDENT { Instance (value,val_type) }
args:
	| { [] }
	| value=expr { [value] }
	| value=expr COMMA values_list=args { value::values_list }
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
