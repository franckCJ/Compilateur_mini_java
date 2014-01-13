let execute lexbuf verbose = 
	try
		Parsing_mjava.code Lexing_mjava.token lexbuf
	with
		(* Gestion des erreurs personnalisées levées par le parser*)
		| Ast.Compilation_Error (kind, pos) ->
			Location.print pos;
			Ast.report_error kind;
			print_newline ();
			[]