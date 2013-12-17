%token CLASS EOF
%token <string> UIDENT
%token <string> LIDENT

%start code

%type <Ast.minijava> code

%%
code:
  |CLASS classname=UIDENT EOF {Class(classname)}
%%
