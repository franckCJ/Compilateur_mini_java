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
  | BOOL of (bool)
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
  | MenhirState105
  | MenhirState98
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState77
  | MenhirState76
  | MenhirState73
  | MenhirState72
  | MenhirState70
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState59
  | MenhirState56
  | MenhirState42
  | MenhirState41
  | MenhirState39
  | MenhirState36
  | MenhirState29
  | MenhirState27
  | MenhirState23
  | MenhirState20
  | MenhirState14
  | MenhirState10
  | MenhirState3
  | MenhirState0

  
let _eRR =
  Error

let rec _menhir_goto_list_sup_args_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_sup_args_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        let _v : (unit) =                   () in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_args_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_args_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLOSEPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, _), _), _), _, _) = _menhir_stack in
        let _v : (unit) =                                           () in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_binop : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | IF ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | INTEGER _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | LIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NEW ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NULL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | OPENPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | STRING _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | THIS ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | UIDENT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_sup_args_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IF ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INTEGER _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NEW ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NULL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | OPENPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | STRING _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | THIS ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | UIDENT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =          () in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =          () in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =        () in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =              () in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =         () in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =          () in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        let _v : (unit) =                           () in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =          () in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =        () in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =          () in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
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
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | IF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | INTEGER _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | LIDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | MINUS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | NEW ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | NOT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | NULL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | OPENPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | STRING _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | THIS ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | UIDENT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | CLOSEPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState56 in
                let _v : (unit option) =     ( None ) in
                _menhir_goto_option_args_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
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

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =        () in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =         () in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLOSEBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, _), _), name), _), _, _) = _menhir_stack in
        let _v : (Ast.minijava) =                                                                                       ( Ast.Method(name, []) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let meth = _v in
        let _v : (Ast.minijava) =                       ( meth ) in
        _menhir_goto_attribute_or_method _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_sup_params_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_sup_params_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _), _) = _menhir_stack in
        let _v : (unit) =                              () in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_params_ _menhir_env _menhir_stack _v
    | _ ->
        _menhir_fail ()

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | IF ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | INTEGER _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | LIDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | NEW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | NOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | NULL ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | OPENPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | STRING _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | THIS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | UIDENT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_unop : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IF ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INTEGER _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NEW ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NULL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | OPENPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | STRING _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | THIS ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | UIDENT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | CLOSEBRACKET | CLOSEPAR | COMMA | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
            let _v : (unit) =              () in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | CLOSEPAR ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | CLOSEPAR | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
            let _v : (unit) =               () in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | CLOSEPAR ->
                _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | CLOSEBRACKET | CLOSEPAR | COMMA | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _), _, _), _, _) = _menhir_stack in
            let _v : (unit) =                    () in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSEPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState70 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | OPENBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FALSE ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | IF ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | INTEGER _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | LIDENT _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | MINUS ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | NEW ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | NOT ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | NULL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | OPENPAR ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | STRING _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | THIS ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | UIDENT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSEBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState73 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
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
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                    | IF ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                    | INTEGER _v ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
                    | LIDENT _v ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
                    | MINUS ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                    | NEW ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                    | NOT ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                    | NULL ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                    | OPENPAR ->
                        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                    | STRING _v ->
                        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
                    | THIS ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                    | TRUE ->
                        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                    | UIDENT _v ->
                        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSEBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState77 in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, _), _), _, _), _), _, _) = _menhir_stack in
            let _v : (unit) =                                                                                              () in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | CLOSEBRACKET | CLOSEPAR | COMMA | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
            let _v : (unit) =                       () in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | CLOSEBRACKET | CLOSEPAR | COMMA | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, _), _, _) = _menhir_stack in
            let _v : (unit) =                                 () in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSEPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState81 in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
            let _v : (unit) =                          () in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState83 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | IF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | INTEGER _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | LIDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | MINUS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | NEW ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | NOT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | NULL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | OPENPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | STRING _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | THIS ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | UIDENT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | CLOSEBRACKET | CLOSEPAR | COMMA | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, _), _), _, _), _), _, _) = _menhir_stack in
            let _v : (unit) =                                      () in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | CLOSEBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (unit option) =     ( Some x ) in
            _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIFF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | EQUAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | INF ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | INFEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | INSTANCEOF ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SEMICOLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SUP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SUPEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | TIMES ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | _ ->
        _menhir_fail ()

and _menhir_goto_attribute_or_method : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.minijava) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STATIC ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | UIDENT _ ->
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | CLOSEBRACKET ->
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_goto_option_params_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLOSEPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | OPENBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | IF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | INTEGER _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | LIDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | MINUS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | NEW ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | NOT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | NULL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | OPENPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | STRING _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | THIS ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | UIDENT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | CLOSEBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState20 in
                let _v : (unit option) =     ( None ) in
                _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
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
        let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_sup_params_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LIDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            let _v : (unit) =                        () in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | CLOSEPAR ->
                _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
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

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LIDENT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =         () in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =         () in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =           () in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | IF ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INTEGER _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NEW ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NULL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | OPENPAR ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | STRING _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | THIS ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState27 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | CLOSEPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | IF ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INTEGER _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | LIDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | MINUS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NEW ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NOT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NULL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | OPENPAR ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | STRING _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | THIS ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | UIDENT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =         () in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =        () in
    _menhir_goto_unop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (unit) =               () in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =          () in
    _menhir_goto_unop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | IF ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | INTEGER _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | LIDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NEW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NULL ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | OPENPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | STRING _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | THIS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | UIDENT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | CLOSEBRACKET | CLOSEPAR | COMMA | DIFF | DIV | DOT | EQUAL | IN | INF | INFEQ | INSTANCEOF | MINUS | PLUS | SEMICOLON | SUP | SUPEQ | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        let _v : (unit) =           () in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =            () in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OPENPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | IF ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | INTEGER _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | LIDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | NEW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | NOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | NULL ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | OPENPAR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | STRING _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | THIS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | UIDENT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =          () in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_list_attribute_or_method_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.minijava list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
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
                let _v : (Ast.minijava) =                                                                                       ( Ast.Class(classname, elements) ) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let current_class = _v in
                let _v : (Ast.minijava) =                                ( current_class ) in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CLASS ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | EOF ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
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
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.minijava list) =     ( x :: xs ) in
        _menhir_goto_list_attribute_or_method_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_STATIC_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
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
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | IF ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | INTEGER _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
                | LIDENT _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
                | MINUS ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | NEW ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | NOT ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | NULL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | OPENPAR ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | STRING _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
                | THIS ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | UIDENT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
            | OPENPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
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
                        | COMMA ->
                            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                        | CLOSEPAR ->
                            _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | CLOSEPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _v : (unit option) =     ( None ) in
                    _menhir_goto_option_params_ _menhir_env _menhir_stack _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (unit option) =     ( None ) in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMICOLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _ = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((_menhir_stack, _menhir_s, _), _), name), _) = _menhir_stack in
                    let _v : (Ast.minijava) =                                                         ( Ast.Attribute(name) ) in
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
            let _v : (Ast.minijava) =                         ( Ast.File(e) ) in
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
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.minijava list) =     ( x :: xs ) in
        _menhir_goto_list_class_or_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.minijava list) =     ( [] ) in
    _menhir_goto_list_attribute_or_method_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_STATIC_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_STATIC_ _menhir_env _menhir_stack _menhir_s _v

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
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.minijava list) =     ( [] ) in
    _menhir_goto_list_class_or_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | UIDENT _ ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | CLOSEBRACKET ->
                _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
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
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)




