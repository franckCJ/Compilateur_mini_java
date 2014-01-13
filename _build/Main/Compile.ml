let execute lexbuf verbose = 
	try
		Parsing_mjava.code Lexing_mjava.token lexbuf;
	with
		| Ast.Compilation_Error (kind, pos) ->
			Location.print pos;
			print_endline ("Erreur");
			print_newline ();
			File []
		| Error ->
			Location.print (Location.curr lexbuf);
			print_endline ("Parsing Error");
			print_newline;
			File []
			
