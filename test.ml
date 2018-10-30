open OUnit2

let make_tokenizer_test name program expected_value =
  name >:: (fun _ ->
    let tokenized_program = Tokenizer.tokenize program in
    assert_equal expected_value tokenized_program
  )

let tokenizer_tests = Token.[
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
    "times float infix expression"
    "1. *. 2."
    [Float 1.; TimesFloat; Float 2.];

  make_tokenizer_test
    "divide infix expression"
    "1 / 2"
    [Int 1; Divide; Int 2];

  make_tokenizer_test
    "negation expression"
    "~-1"
    [Negation; Int 1;];

  make_tokenizer_test
    "empty list constant"
    "[]"
    [EmptyList];

  make_tokenizer_test
    "cons operator"
    "1::[]"
    [Int 1; Cons; EmptyList];

  make_tokenizer_test
    "list append operator"
    "[]@[]"
    [EmptyList; Append; EmptyList];

  make_tokenizer_test
    "list literal"
    "[1;2;4]"
    [StartList; Int 1; SemiColon; Int 2; SemiColon; Int 4; EndList];

  make_tokenizer_test
    "module accessor"
    "List.length"
    [CapitalizedIdent "List"; Period; LowercaseIdent "length"];

  make_tokenizer_test
    "negation float expression"
    "~-.1."
    [NegationFloat; Float 1.];

  make_tokenizer_test
    "double semicolon expression"
    ";;"
    [DoubleSemicolon];
]

let make_parser_test name program expected_tree =
  name >:: (fun _ ->
    let parse_tr =
      Tokenizer.tokenize program
      |> Array.of_list
      |> Parser.parse
    in
    assert_equal expected_tree parse_tr
  )

let parser_tests = Parser.(Tokenizer.[
  make_parser_test
    "append three lists, right associativity"
    "[]@[]@[]"
    (Node [
      Token (EmptyList);
      Token (Append);
      Node [
        Token (EmptyList);
        Token (Append);
        Token (EmptyList);
      ]
    ]);

  make_parser_test
    "cons three elemenets, right associativity"
    "1::2::[]"
    (Node [
      Token (Int 1);
      Token (Cons);
      Node [
        Token (Int 2);
        Token (Cons);
        Token (EmptyList);
      ];
    ]);

  make_parser_test
    "append, cons precedence over comp expr, under add expr"
    "1::2+3@3::4 = []"
    (Node [
      Node [
        Node [
          Token (Int 1);
          Token (Cons);
          Node [
            Token (Int 2);
            Token (Plus);
            Token (Int 3);
          ];
        ];
        Token (Append);
        Node [
          Token (Int 3);
          Token (Cons);
          Token (Int 4);
        ];
      ];
      Token (Equal);
      Token (EmptyList);
    ]);

  make_parser_test
    "list literal, no trailing semicolon"
    "[1;2;3]"
    (Node [
      Token (StartList);
      Node [
        Node [
          Token (Int 1);
          Token (SemiColon);
          Token (Int 2);
        ];
        Token (SemiColon);
        Token (Int 3);
      ];
      Token (EndList);
    ]);

  make_parser_test
    "list literal, trailing semicolon"
    "[1;2;3;]"
    (Node [
      Token (StartList);
      Node [
        Node [
          Token (Int 1);
          Token (SemiColon);
          Token (Int 2);
        ];
        Token (SemiColon);
        Token (Int 3);
      ];
      Token (SemiColon);
      Token (EndList);
    ]);

  make_parser_test
    "list literal, precedence over function calls"
    "f [~-1; 0]"
    (Node [
      Token (LowercaseIdent "f");
      Node [
        Token (StartList);
        Node [
          Node [
            Token (Negation);
            Token (Int 1);
          ];
          Token (SemiColon);
          Token (Int 0);
        ];
        Token (EndList);
      ];
    ]);

  make_parser_test
    "module accessor, parse tree"
    "List.length []"
    (Node [
      Node [
        Token (CapitalizedIdent "List");
        Token (Period);
        Token (LowercaseIdent "length");
      ];
      Token (EmptyList);
    ]);
])

let make_ast_converter_test name program expected_tree =
  name >:: (fun _ ->
    let ast =
      Tokenizer.tokenize program
      |> Array.of_list
      |> Parser.parse
      |> Ast_converter.convert
    in
    assert_equal expected_tree ast
  )

let ast_converter_tests = Ast.[
  make_ast_converter_test
    "basic_test"
    "let x = 1 in x + 100"
    (LetBinding (
      VarAssignment (
        ValueName "x",
        Constant (Int 1)
      ),
      InfixOp (
        VarName "x",
        Plus,
        Constant (Int 100)
      )
    ));

  make_ast_converter_test
    "order of ops in infix expression"
    "let x = 1 in x - 100 - 200"
    (LetBinding (
        VarAssignment (
          ValueName "x",
          Constant (Int 1)
        ),
        InfixOp (
          InfixOp (
            VarName "x",
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
          ValueName "x",
          Constant (Int 1)
        ),
        InfixOp (
          ParenExpr (InfixOp (
            VarName "x",
            Minus,
            Constant (Int 100)
          )),
          Minus,
          ParenExpr (InfixOp (
            Constant (Int 200),
            Minus,
            VarName "x"
          ))
        )));

  make_ast_converter_test
    "order of ops in infix expression parentheiszed nested"
    "let x = 1 in (x - 100 - 300) - (200 - x)"
    (LetBinding (
          VarAssignment (
            ValueName "x",
            Constant (Int 1)
          ),
          InfixOp (
            ParenExpr (
              InfixOp (
                InfixOp (
                  VarName "x",
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
              VarName "x"
            ))
          )
        ));

  make_ast_converter_test
    "function call"
    "let rec x a b c = a * b * c in x 1 2 3"
    (LetBinding (
      FunctionAssignment (
        "x",
        true,
        [
          ValueName "a";
          ValueName "b";
          ValueName "c";
        ],
        InfixOp (
          InfixOp (
            VarName "a",
            Times,
            VarName "b"
          ),
          Times,
          VarName "c"
        )
      ),
      FunctionCall (
        FunctionCall (
          FunctionCall (
            VarName "x",
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
      [ValueName "a"; ValueName "b"],
      InfixOp (
        VarName "a", Plus, VarName "b"
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

  make_ast_converter_test
    "empty list"
    "[]"
    (Constant (EmptyList));

  make_ast_converter_test
    "list literal, no trailing semicolon"
    "[1;2;3]"
    (ListExpr [
      Constant (Int 1);
      Constant (Int 2);
      Constant (Int 3);
    ]);

  make_ast_converter_test
    "list literal, trailing semicolon"
    "[1;2;3;]"
    (ListExpr [
      Constant (Int 1);
      Constant (Int 2);
      Constant (Int 3);
    ]);

  make_ast_converter_test
    "cons operator"
    "1::2::[]"
    (InfixOp (
      Constant (Int 1),
      Cons,
      InfixOp (
        Constant (Int 2),
        Cons,
        Constant (EmptyList)
      )
    ));

  make_ast_converter_test
    "append operator"
    "[1;2;]@[3]"
    (InfixOp (
      ListExpr [
        Constant (Int 1);
        Constant (Int 2);
      ],
      Append,
      ListExpr [
        Constant (Int 3);
      ]
    ));

  make_ast_converter_test
    "module accessor, list length of empty list"
    "List.length []"
    (FunctionCall (
      ModuleAccessor ("List", "length"),
      Constant (EmptyList)
    ));

  make_ast_converter_test
    "module accessor, higher precedence than function call, higher than prefix"
    "List.create ~-List.empty_size"
    (FunctionCall (
      ModuleAccessor ("List", "create"),
      PrefixOp (
        Negation,
        ModuleAccessor ("List", "empty_size")
      )
    ));

  make_ast_converter_test
      "let-rec-in fn (factorial) featuring if-else-then"
      "let rec fact x = if x=1 then 1 else x * fact(x-1) in fact 3"
      (LetBinding (
          FunctionAssignment (
            "fact",
            true,
            [ValueName "x";],
            Ternary (
              InfixOp(
                VarName "x",
                Equal,
                Constant (Int 1)
              ),
              Constant (Int 1),
              Some (
                InfixOp(
                  VarName "x",
                  Times,
                  FunctionCall(
                    VarName "fact",
                    ParenExpr (
                      InfixOp(
                        VarName "x",
                        Minus,
                        Constant (Int 1)
                      )
                    )
                  )
                )
              )
            )
           ),
          FunctionCall(
            VarName "fact",
            Constant (Int 3)
          )
        ));
]

let suite = "test suite"  >::: List.flatten [
  tokenizer_tests;
  parser_tests;
  ast_converter_tests;
]

let _ = run_test_tt_main suite
