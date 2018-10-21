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
  )
]

let suite = "test suite"  >::: List.flatten [
  tokenizer_tests;
  parser_tests;
]

let _ = run_test_tt_main suite
