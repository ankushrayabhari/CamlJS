open OUnit2
open Tokenizer

let tokenizer_tests = [
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

let suite = "test suite"  >::: List.flatten [
  tokenizer_tests;
]

let _ = run_test_tt_main suite
