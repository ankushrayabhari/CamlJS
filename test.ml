open OUnit2

let tokenizer_tests = Tokenizer.[
  "basic_test" >:: (fun _ ->
    let tokenized_program = tokenize "let x = 1 in x + 100" in
    let expected_tokens = [
      Let;
      LowercaseIdent "x";
      Equal;
      Int 1;
      In;
      LowercaseIdent "x";
      Plus;
      Int 100;
    ] in
    assert_equal expected_tokens tokenized_program
  )
]

let parser_tests = Parser.[
  "basic_test" >:: (fun _ ->
    let tokenized_program = Tokenizer.tokenize "let x = 1 in x + 100" |> Array.of_list in
    let parse_tree = parse tokenized_program in
    let expected_tree =
      LetBinding (
        VarAssignment (
          ValueName (LowercaseIdent "x"),
          Constant (Int 1)
        ),
        InfixOp (
          VarName (LowercaseIdent "x"),
          Plus,
          Constant (Int 100)
        )
      ) in
    assert_equal expected_tree parse_tree
  );

  "order of ops in infix expression" >:: (fun _ ->
    let tokenized_program = Tokenizer.tokenize "let x = 1 in x - 100 - 200"
                            |> Array.of_list in
    let parse_tree = parse tokenized_program in
    let expected_tree =
      LetBinding (
        VarAssignment (
          ValueName (LowercaseIdent "x"),
          Constant (Int 1)
        ),
        InfixOp (
          InfixOp (
            VarName (LowercaseIdent "x"),
            Minus,
            Constant (Int 100)
          ),
          Minus,
          Constant (Int 200)
        )
      ) in
    assert_equal expected_tree parse_tree
  );

  "order of ops in infix expression parentheiszed" >:: (fun _ ->
    let tokenized_program = Tokenizer.tokenize "let x = 1 in (x - 100) - (200 - x)"
                            |> Array.of_list in
    let parse_tree = parse tokenized_program in
    let expected_tree =
      LetBinding (
        VarAssignment (
          ValueName (LowercaseIdent "x"),
          Constant (Int 1)
        ),
        InfixOp (
          ParenExpr (InfixOp (
            VarName (LowercaseIdent "x"),
            Minus,
            Constant (Int 100)
          )),
          Minus,
          ParenExpr (InfixOp (
            Constant (Int 200),
            Minus,
            VarName (LowercaseIdent "x")
          ))
        )
      ) in
    assert_equal expected_tree parse_tree
  );
]

let renderer_tests = Renderer.[
  "basic_test" >:: (fun _ ->
      Tokenizer.tokenize "let x = 1 in x + 100"
      |> Array.of_list
      |> Parser.parse
      |> render
      |> assert_equal
          "(() => {let x = (() => 1)();return (() => (x)+((() => 100)()))()})()"
  )
]

let suite = "test suite"  >::: List.flatten [
  tokenizer_tests;
  parser_tests;
  renderer_tests;
]

let _ = run_test_tt_main suite
