%{
type Class of string
%}

%token CLASS EOF
%token <string> UIDENT
%token <string> LIDENT

%start minijava

(*%type <*> minijava*)

%%
minijava:
  |CLASS classname=UIDENT {Class(classname)}
%%
