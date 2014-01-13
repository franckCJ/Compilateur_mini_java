let execute lexbuf verbose = 
	try
		let ast = Parsing_mjava.code Lexing_mjava.token lexbuf in
		Printf.printf ast
	with
		(* Gestion des erreurs personnalisées levées par le parser*)
		| Ast.Compilation_Error (kind, pos) ->
			Location.print pos;
			Ast.report_error kind;
			print_newline ();
			(*Ast.File []*)
