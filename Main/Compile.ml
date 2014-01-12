let execute lexbuf verbose = 
	try
		Parsing_mjava.code Lexing_mjava.token lexbuf;
	with
		| Ast.Error (kind, pos) ->
			Location.print pos;
			print_endline ("Erreur");
			print_newline ();
			File []
			
