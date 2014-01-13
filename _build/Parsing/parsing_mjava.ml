exception Error

type token = 
  | UIDENT of (string)
  | TRUE
  | TIMES
  | THIS
  | SUPEQ
  | SUP
  | STRING of (string)
  | STATIC
  | SEMICOLON
  | PLUS
  | OR
  | OPENPAR
  | OPENBRACKET
  | NULL
  | NOT
  | NEW
  | MODULO
  | MINUS
  | LIDENT of (string)
  | INTEGER of (int)
  | INSTANCEOF
  | INFEQ
  | INF
  | IN
  | IF
  | FALSE
  | EQUAL
  | EOF
  | ELSE
  | DOT
  | DIV
  | DIFF
  | COMMA
  | CLOSEPAR
  | CLOSEBRACKET
  | CLASS
  | ASSIGN
  | AND

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState98
  | MenhirState90
  | MenhirState82
  | MenhirState79
  | MenhirState75
  | MenhirState72
  | MenhirState67
  | MenhirState63
  | MenhirState55
  | MenhirState51
  | MenhirState45
  | MenhirState42
  | MenhirState38
  | MenhirState21
  | MenhirState19
  | MenhirState16
  | MenhirState9
  | MenhirState7
  | MenhirState3
  | MenhirState0

  
	open Ast
let _eRR =
  Error

let rec _menhir_goto_params : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.param list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, val_type), name), _, param_list) = _menhir_stack in
        let _v : (Ast.param list) =                                                        ( (Param (name,val_type))::param_list ) in
        _menhir_goto_params _menhir_env _menhir_stack _menhir_s _v
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSEPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | OPENBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FALSE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | IF ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | INTEGER _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                | LIDENT _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                | MINUS ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | NEW ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | NOT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | NULL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | OPENPAR ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | STRING _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                | THIS ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | UIDENT _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_binop : _menhir_env -> 'ttv_tail -> (Ast.binop) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INTEGER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LIDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | MINUS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NULL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | OPENPAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | THIS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_reduce48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.param list) =    ( [] ) in
    _menhir_goto_params _menhir_env _menhir_stack _menhir_s _v

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | UIDENT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | CLOSEPAR ->
                _menhir_reduce48 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | CLOSEPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, val_type), name) = _menhir_stack in
            let _v : (Ast.param list) =                                ( [Param (name,val_type)] ) in
            _menhir_goto_params _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_class_or_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.minijava) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | INTEGER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | LIDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | MINUS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | NULL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | OPENPAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | THIS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | EOF ->
        _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_goto_attribute_or_method : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.minijava) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STATIC ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | UIDENT _ ->
        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | CLOSEBRACKET ->
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, value), _, values_list) = _menhir_stack in
        let _v : (Ast.expression list) =                                      ( value::values_list ) in
        _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSEPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, container), method_name), _, arg_list) = _menhir_stack in
            let _v : (Ast.expression) =                                                                         ( Method_call (container,method_name,arg_list) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expression list) =    ( [] ) in
    _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =          ( Bmul ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run24 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =          ( Bsupeq ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =        ( Bsup ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =              ( Bdel ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run27 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =         ( Badd ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run28 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =       ( Bor ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =           ( Bmod ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run30 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =          ( Bsub ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let val_type = _v in
        let (_menhir_stack, _menhir_s, value) = _menhir_stack in
        let _v : (Ast.expression) =                                          ( Instance (value,val_type) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run33 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =          ( Binfeq ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =        ( Binf ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =          ( Bequal ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | OPENPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | INTEGER _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | LIDENT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | MINUS ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | NULL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | OPENPAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | THIS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | UIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | CLOSEPAR ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run40 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =        ( Bdiv ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run41 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =         ( Bdiff ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run44 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.binop) =        ( Band ) in
    _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_goto_list_attribute_or_method_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.minijava list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSEBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), classname), _, elements) = _menhir_stack in
                let _v : (Ast.minijava) =                                                                                       ( Class(classname, elements) ) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let current_class = _v in
                let _v : (Ast.minijava) =                                ( current_class ) in
                _menhir_goto_class_or_expr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.minijava list) =     ( x :: xs ) in
        _menhir_goto_list_attribute_or_method_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_static : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LIDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ASSIGN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FALSE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | IF ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | INTEGER _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
                | LIDENT _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
                | MINUS ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | NEW ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | NOT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | NULL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | OPENPAR ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | STRING _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
                | THIS ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | UIDENT _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
            | OPENPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | UIDENT _v ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | CLOSEPAR ->
                    _menhir_reduce48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (Ast.expression) =    ( None ) in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMICOLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _ = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((_menhir_stack, _menhir_s, stat), val_type), name), value) = _menhir_stack in
                    let _v : (Ast.minijava) =                                                                           ( Attribute (name,val_type,stat,value) ) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let attr = _v in
                    let _v : (Ast.minijava) =                    ( attr ) in
                    _menhir_goto_attribute_or_method _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_class_or_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.minijava list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, e) = _menhir_stack in
            let _v : (Ast.minijava) =                         ( File(e) ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.minijava list) =     ( x :: xs ) in
        _menhir_goto_list_class_or_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | INTEGER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | LIDENT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | NULL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | OPENPAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | THIS ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | UIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_unop : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.unop) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | INTEGER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LIDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | MINUS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NULL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | OPENPAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | THIS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | CLASS | CLOSEBRACKET | CLOSEPAR | COMMA | EOF | FALSE | IF | IN | INTEGER _ | LIDENT _ | NEW | NOT | NULL | OPENPAR | STRING _ | THIS | TRUE | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, op), _, value) = _menhir_stack in
            let _v : (Ast.expression) =                       ( Unop (op,value) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | INTEGER _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | LIDENT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | MINUS ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | NULL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | OPENPAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | THIS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | UIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | CLOSEPAR ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | CLOSEPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, value) = _menhir_stack in
            let _v : (Ast.expression list) =               ( [value] ) in
            _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | CLASS | CLOSEBRACKET | CLOSEPAR | COMMA | EOF | FALSE | IF | IN | INTEGER _ | LIDENT _ | NEW | NOT | NULL | OPENPAR | STRING _ | THIS | TRUE | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, value1), op), _, value2) = _menhir_stack in
            let _v : (Ast.expression) =                                     ( Binop (op,value1,value2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | CLOSEPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | OPENBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FALSE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | IF ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | INTEGER _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | LIDENT _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | MINUS ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | NEW ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | NOT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | NULL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | OPENPAR ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | STRING _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | THIS ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | UIDENT _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | CLOSEBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | OPENBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | FALSE ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                    | IF ->
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                    | INTEGER _v ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
                    | LIDENT _v ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
                    | MINUS ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                    | NEW ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                    | NOT ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                    | NULL ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                    | OPENPAR ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                    | STRING _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
                    | THIS ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                    | TRUE ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                    | UIDENT _v ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | CLOSEBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, expr_if), _, expr_then), _, expr_else) = _menhir_stack in
            let _v : (Ast.expression) =                                                                                                                          ( Condition (expr_if,expr_then, expr_else) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | CLASS | CLOSEBRACKET | CLOSEPAR | COMMA | EOF | FALSE | IF | IN | INTEGER _ | LIDENT _ | NEW | NOT | NULL | OPENPAR | STRING _ | THIS | TRUE | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, name), _, value) = _menhir_stack in
            let _v : (Ast.expression) =                                  ( Assignment (name,value) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | CLASS | CLOSEBRACKET | CLOSEPAR | COMMA | EOF | FALSE | IF | IN | INTEGER _ | LIDENT _ | NEW | NOT | NULL | OPENPAR | STRING _ | THIS | TRUE | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, val_type), _, value) = _menhir_stack in
            let _v : (Ast.expression) =                                                ( Cast (value,val_type) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | CLOSEPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, value) = _menhir_stack in
            let _v : (Ast.expression) =                                ( value ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | INTEGER _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | LIDENT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | MINUS ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NULL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | OPENPAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | THIS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | UIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | CLASS | CLOSEBRACKET | CLOSEPAR | COMMA | EOF | FALSE | IF | IN | INTEGER _ | LIDENT _ | NEW | NOT | NULL | OPENPAR | STRING _ | THIS | TRUE | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, val_type), name), _, value), _, target) = _menhir_stack in
            let _v : (Ast.expression) =                                                                 ( Locassign (name,val_type,value,target) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | CLOSEBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, stat), val_type), name), _, param_list), _, body) = _menhir_stack in
            let _v : (Ast.minijava) =                                                                                                                  ( Method(name,val_type,stat,param_list,body) ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let meth = _v in
            let _v : (Ast.minijava) =                       ( meth ) in
            _menhir_goto_attribute_or_method _menhir_env _menhir_stack _menhir_s _v
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIFF ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | CLASS | EOF | FALSE | IF | INTEGER _ | LIDENT _ | NEW | NOT | NULL | OPENPAR | STRING _ | THIS | TRUE | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, current_expr) = _menhir_stack in
            let _v : (Ast.minijava) =                      ( Expression current_expr ) in
            _menhir_goto_class_or_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.minijava list) =     ( [] ) in
    _menhir_goto_list_attribute_or_method_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bool) =    ( false ) in
    _menhir_goto_static _menhir_env _menhir_stack _menhir_s _v

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (bool) =            ( true ) in
    _menhir_goto_static _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.minijava list) =     ( [] ) in
    _menhir_goto_list_class_or_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expression) =         ( Boolean true ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expression) =         ( Self ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let value = _v in
    let _v : (Ast.expression) =                 ( String value ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | INTEGER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LIDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | MINUS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | NULL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | OPENPAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | THIS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState7 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | CLOSEPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | INTEGER _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | LIDENT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | MINUS ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | NULL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | OPENPAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | THIS ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | UIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
        | LIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expression) =         ( None ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.unop) =        ( Unot ) in
    _menhir_goto_unop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let val_type = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.expression) =                        ( Object val_type ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.unop) =                       ( Uopposite ) in
    _menhir_goto_unop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | INTEGER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | LIDENT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | NULL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | OPENPAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | THIS ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | UIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
    | AND | CLASS | CLOSEBRACKET | CLOSEPAR | COMMA | DIFF | DIV | DOT | EOF | EQUAL | FALSE | IF | IN | INF | INFEQ | INSTANCEOF | INTEGER _ | LIDENT _ | MINUS | MODULO | NEW | NOT | NULL | OPENPAR | OR | PLUS | SEMICOLON | STRING _ | SUP | SUPEQ | THIS | TIMES | TRUE | UIDENT _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, name) = _menhir_stack in
        let _v : (Ast.expression) =                ( Variable name ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let value = _v in
    let _v : (Ast.expression) =                  ( Integer value ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OPENPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | INTEGER _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | LIDENT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | NULL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | OPENPAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | THIS ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | UIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expression) =          ( Boolean false ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | OPENBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | STATIC ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | UIDENT _ ->
                _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | CLOSEBRACKET ->
                _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and code : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.minijava) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = max_int;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INTEGER _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LIDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | MINUS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NULL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | OPENPAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | THIS ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | EOF ->
        _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)




