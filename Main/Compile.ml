let execute lexbuf verbose = 
  try 
    let ast = Parser.start Lexer.token lexbuf in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast;
		let typed_ast = Typer.type_program ast in
		print_endline "successfull typing";
		if verbose then AST.print_program typed_ast;
		let compiled_prog = Compiler.compile_program typed_ast in
		print_endline "successfull compiling";
    if verbose then Compiler.print_program compiled_prog;
		Execute.evaluate_program compiled_prog;
		print_endline "successfull execution";
  with 
    | Parser.Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
    | TypeError.Error(e,l) ->
      TypeError.report_error e;
      Location.print l
    | ExecError.Error(e,l) ->
			ExecError.report_error e;
			Location.print l
