open OUnit2

let make_tokenizer_test name program expected_value =
  name >:: (fun _ ->
    let tokenized_program = Tokenizer.tokenize program in
    assert_equal expected_value tokenized_program
  )

let tokenizer_tests = Tokenizer.[
  make_tokenizer_test
    "let assign test"
    "let x = 1 in x + 100"
    [Let; LowercaseIdent "x"; Equal; Int 1; In; LowercaseIdent "x"; Plus;
     Int 100;];

  make_tokenizer_test
    "let rec function test with paren expression"
    "let rec sum x = x + sum (x - 1)"
    [Let; Rec; LowercaseIdent "sum"; LowercaseIdent "x"; Equal;
     LowercaseIdent "x"; Plus; LowercaseIdent "sum"; LParen;
     LowercaseIdent "x"; Minus; Int 1; RParen;];

  make_tokenizer_test
    "greater than comparison expression"
    "1 > 2"
    [Int 1; GreaterThan; Int 2];

  make_tokenizer_test
    "less than comparison expression"
    "1 < 2"
    [Int 1; LessThan; Int 2];

  make_tokenizer_test
    "greater than or equal comparison expression"
    "1 >= 2"
    [Int 1; GreaterThanOrEqual; Int 2];

  make_tokenizer_test
    "less than or equal comparison expression"
    "1 <= 2"
    [Int 1; LessThanOrEqual; Int 2];

  make_tokenizer_test
    "is equal comparison expression"
    "1 = 2"
    [Int 1; Equal; Int 2];

  make_tokenizer_test
    "not equal comparison expression"
    "1 <> 2"
    [Int 1; NotEqual; Int 2];

  make_tokenizer_test
    "if then no else expression"
    "if 1 = 2 then 0"
    [If; Int 1; Equal; Int 2; Then; Int 0];

  make_tokenizer_test
    "if then with else expression"
    "if 1 = 2 then 0 else 5"
    [If; Int 1; Equal; Int 2; Then; Int 0; Else; Int 5];

  make_tokenizer_test
    "if statement test"
    "if 0 <> 0 then 0 else 1"
    [If; Int 0; NotEqual; Int 0; Then; Int 0; Else; Int 1; ];

  make_tokenizer_test
    "anonymous function expression"
    "fun a b -> a + b"
    [Fun; LowercaseIdent "a"; LowercaseIdent "b"; FunctionArrow;
     LowercaseIdent "a"; Plus; LowercaseIdent "b"];

  make_tokenizer_test
    "semicolon expression"
    "1;2"
    [Int 1; SemiColon; Int 2];

  make_tokenizer_test
    "times infix expression"
    "1 * 2"
    [Int 1; Times; Int 2];

  make_tokenizer_test
    "divide infix expression"
    "1 / 2"
    [Int 1; Divide; Int 2];

  make_tokenizer_test
    "negation expression"
    "~-1"
    [Negation; Int 1;];
]

let make_ast_converter_test name program expected_tree =
  name >:: (fun _ ->
    let ast =
      Tokenizer.tokenize program
      |> Array.of_list
      |> Parser.parse
      |> Ast.convert
    in
    assert_equal expected_tree ast
  )

let ast_converter_tests = Ast.[
  make_ast_converter_test
    "basic_test"
    "let x = 1 in x + 100"
    (LetBinding (
      VarAssignment (
        ValueName (LowercaseIdent "x"),
        Constant (Int 1)
      ),
      InfixOp (
        VarName (LowercaseIdent "x"),
        Plus,
        Constant (Int 100)
      )
    ));

  make_ast_converter_test
    "order of ops in infix expression"
    "let x = 1 in x - 100 - 200"
    (LetBinding (
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
      ));

  make_ast_converter_test
    "order of ops in infix expression parentheiszed"
    "let x = 1 in (x - 100) - (200 - x)"
    (LetBinding (
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
        )));

  make_ast_converter_test
    "order of ops in infix expression parentheiszed nested"
    "let x = 1 in (x - 100 - 300) - (200 - x)"
    (LetBinding (
          VarAssignment (
            ValueName (LowercaseIdent "x"),
            Constant (Int 1)
          ),
          InfixOp (
            ParenExpr (
              InfixOp (
                InfixOp (
                  VarName (LowercaseIdent "x"),
                  Minus,
                  Constant (Int 100)
                ),
                Minus,
                Constant (Int 300)
              )
            ),
            Minus,
            ParenExpr (InfixOp (
              Constant (Int 200),
              Minus,
              VarName (LowercaseIdent "x")
            ))
          )
        ));

  make_ast_converter_test
    "function call"
    "let rec x a b c = a * b * c in x 1 2 3"
    (LetBinding (
      FunctionAssignment (
        LowercaseIdent "x",
        true,
        [
          ValueName (LowercaseIdent "a");
          ValueName (LowercaseIdent "b");
          ValueName (LowercaseIdent "c");
        ],
        InfixOp (
          InfixOp (
            VarName (LowercaseIdent "a"),
            Times,
            VarName (LowercaseIdent "b")
          ),
          Times,
          VarName (LowercaseIdent "c")
        )
      ),
      FunctionCall (
        FunctionCall (
          FunctionCall (
            VarName (LowercaseIdent "x"),
            Constant (Int 1)
          ),
          Constant (Int 2)
        ),
        Constant (Int 3)
      )
    ));

  make_ast_converter_test
    "greater than comparison expression"
    "1 > 2"
    (InfixOp (Constant (Int 1), GreaterThan, (Constant (Int 2))));

  make_ast_converter_test
    "less than comparison expression"
    "1 < 2"
    (InfixOp (Constant (Int 1), LessThan, (Constant (Int 2))));

  make_ast_converter_test
    "less than or equal comparison expression"
    "1 <= 2"
    (InfixOp (Constant (Int 1), LessThanOrEqual, (Constant (Int 2))));

  make_ast_converter_test
    "greater than or equal comparison expression"
    "1 >= 2"
    (InfixOp (Constant (Int 1), GreaterThanOrEqual, (Constant (Int 2))));

  make_ast_converter_test
    "is equal comparison expression"
    "1 = 2"
    (InfixOp (Constant (Int 1), Equal, (Constant (Int 2))));

  make_ast_converter_test
    "not equal comparison expression"
    "1 <> 2"
    (InfixOp (Constant (Int 1), NotEqual, (Constant (Int 2))));

  make_ast_converter_test
    "if then no else expression"
    "if 1 = 2 then 0"
    (Ternary (
      InfixOp (Constant (Int 1), Equal, (Constant (Int 2))),
      Constant (Int 0),
      None));

  make_ast_converter_test
    "if then with else expression"
    "if 1 = 2 then 0 else 5"
    (Ternary (
      InfixOp (Constant (Int 1), Equal, (Constant (Int 2))),
      Constant (Int 0),
      Some (Constant (Int 5))));

  make_ast_converter_test
    "anonymous function expression"
    "fun a b -> a + b"
    (Function (
      [ValueName (LowercaseIdent "a"); ValueName (LowercaseIdent "b")],
      InfixOp (
        VarName (LowercaseIdent "a"), Plus, VarName (LowercaseIdent "b")
      ))
    );

  make_ast_converter_test
    "semicolon expression"
    "1;2"
    (Sequential (Constant (Int 1), Constant (Int 2)));

  make_ast_converter_test
    "times infix expression"
    "1 * 2"
    (InfixOp (Constant (Int 1), Times, (Constant (Int 2))));

  make_ast_converter_test
    "divide infix expression"
    "1 / 2"
    (InfixOp (Constant (Int 1), Divide, (Constant (Int 2))));

  make_ast_converter_test
    "negation expression"
    "~-1"
    (PrefixOp (Negation, Constant (Int 1)));
]

let suite = "test suite"  >::: List.flatten [
  tokenizer_tests;
  ast_converter_tests;
]

let _ = run_test_tt_main suite
