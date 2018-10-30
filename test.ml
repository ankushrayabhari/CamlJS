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

  make_tokenizer_test
    "open module"
    "open List"
    [Open; CapitalizedIdent "List"];

  make_tokenizer_test
    "simple bool test, tokenizer"
    "let x = true in ()"
    [Let; LowercaseIdent "x"; Equal; Bool true; In; LParen; RParen];

  make_tokenizer_test
    "and test, tokenizer"
    "let x = (true && false)"
    [Let; LowercaseIdent "x"; Equal; LParen; Bool true; And; Bool false; RParen];

  make_tokenizer_test
    "or test, tokenizer"
    "let bool_expr = true || false in \nlet x = 1"
    [Let; LowercaseIdent "bool_expr"; Equal; Bool true; Or; Bool false; In; Let; LowercaseIdent "x"; Equal; Int 1];

  make_tokenizer_test
    "not test, tokenizer"
    "let x = not false"
    [Let; LowercaseIdent "x"; Equal; LowercaseIdent "not"; Bool false];

  make_tokenizer_test
    "complicated and/or/not test, tokenizer"
    "let x = not (true && false)"
    [Let; LowercaseIdent "x"; Equal; LowercaseIdent "not"; LParen; Bool true; And; Bool false; RParen];
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

  make_parser_test
    "compilation unit, start with ;;, single expr"
    ";;1+1"
    (Node [
      Token (DoubleSemicolon);
      Node [
        Token (Int 1);
        Token (Plus);
        Token (Int 1);
      ];
    ]);

  make_parser_test
    "compilation unit, end with ;;, single expr"
    "1+1;;"
    (Node [
      Node [
        Token (Int 1);
        Token (Plus);
        Token (Int 1);
      ];
      Token (DoubleSemicolon);
    ]);

  make_parser_test
    "compilation unit, start/end with ;;, single expr"
    ";;1+1;;"
    (Node [
      Token (DoubleSemicolon);
      Node [
        Node [
          Token (Int 1);
          Token (Plus);
          Token (Int 1);
        ];
        Token (DoubleSemicolon);
      ]
    ]);

  make_parser_test
    "compilation unit, start/end with ;;, double expr"
    ";;1+1;;1+1;;"
    (Node [
      Token (DoubleSemicolon);
      Node [
        Node [
          Token (Int 1);
          Token (Plus);
          Token (Int 1);
        ];
        Node [
          Token (DoubleSemicolon);
          Node [
            Token (Int 1);
            Token (Plus);
            Token (Int 1);
          ];
        ];
        Token (DoubleSemicolon);
      ]
    ]);

  make_parser_test
    "compilation unit, start with ;;, double expr"
    ";;1+1;;1+1"
    (Node [
      Token (DoubleSemicolon);
      Node [
        Node [
          Token (Int 1);
          Token (Plus);
          Token (Int 1);
        ];
        Node [
          Token (DoubleSemicolon);
          Node [
            Token (Int 1);
            Token (Plus);
            Token (Int 1);
          ];
        ];
      ]
    ]);

  make_parser_test
    "compilation unit, end with ;;, double expr"
    "1+1;;1+1;;"
    (Node [
      Node [
        Token (Int 1);
        Token (Plus);
        Token (Int 1);
      ];
      Node [
        Token (DoubleSemicolon);
        Node [
          Token (Int 1);
          Token (Plus);
          Token (Int 1);
        ];
      ];
      Token (DoubleSemicolon);
    ]);

  make_parser_test
    "compilation unit, end with ;;, double expr"
    "1+1;;1+1;;"
    (Node [
      Node [
        Token (Int 1);
        Token (Plus);
        Token (Int 1);
      ];
      Node [
        Token (DoubleSemicolon);
        Node [
          Token (Int 1);
          Token (Plus);
          Token (Int 1);
        ];
      ];
      Token (DoubleSemicolon);
    ]);

  make_parser_test
    "compilation unit, start with ;;, single Definition"
    ";;open List"
    (Node [
      Token (DoubleSemicolon);
      Node [
        Token (Open);
        Token (CapitalizedIdent "List");
      ];
    ]);

  make_parser_test
    "compilation unit, end with ;;, single expr"
    "let rec x = 1;;"
    (Node [
      Node [
        Token (Let);
        Token (Rec);
        Node [
          Token (LowercaseIdent "x");
          Token (Equal);
          Token (Int 1);
        ];
      ];
      Token (DoubleSemicolon);
    ]);

  make_parser_test
    "compilation unit, start/end with ;;, single expr"
    ";;let rec x = 1;;"
    (Node [
      Token (DoubleSemicolon);
      Node [
        Node [
          Token (Let);
          Token (Rec);
          Node [
            Token (LowercaseIdent "x");
            Token (Equal);
            Token (Int 1);
          ];
        ];
        Token (DoubleSemicolon);
      ]
    ]);

  make_parser_test
    "compilation unit, start/end with ;;, double definition"
    ";;let x = 1;;let rec x = 1;;"
    (Node [
      Token (DoubleSemicolon);
      Node [
        Node [
          Token (Let);
          Node [
            Token (LowercaseIdent "x");
            Token (Equal);
            Token (Int 1);
          ];
        ];
        Node [
          Token (DoubleSemicolon);
          Node [
            Token (Let);
            Token (Rec);
            Node [
              Token (LowercaseIdent "x");
              Token (Equal);
              Token (Int 1);
            ];
          ];
        ];
        Token (DoubleSemicolon);
      ]
    ]);

  make_parser_test
    "compilation unit, start with ;;, double definition"
    ";;let x = 1;;let x = 1"
    (Node [
      Token (DoubleSemicolon);
      Node [
        Node [
          Token (Let);
          Node [
            Token (LowercaseIdent "x");
            Token (Equal);
            Token (Int 1);
          ];
        ];
        Node [
          Token (DoubleSemicolon);
          Node [
            Token (Let);
            Node [
              Token (LowercaseIdent "x");
              Token (Equal);
              Token (Int 1);
            ];
          ];
        ];
      ]
    ]);

  make_parser_test
    "compilation unit, end with ;;, double definition"
    "open List;;let x = 1;;"
    (Node [
      Node [
        Token (Open);
        Token (CapitalizedIdent "List");
      ];
      Node [
        Token (DoubleSemicolon);
        Node [
          Token (Let);
          Node [
            Token (LowercaseIdent "x");
            Token (Equal);
            Token (Int 1);
          ];
        ];
      ];
      Token (DoubleSemicolon);
    ]);

  make_parser_test
    "compilation unit, no ;;, double definition"
    "let x = 1\nlet x = 1"
    (Node [
      Node [
        Token (Let);
        Node [
          Token (LowercaseIdent "x");
          Token (Equal);
          Token (Int 1);
        ];
      ];
      Node [
        Token (Let);
        Node [
          Token (LowercaseIdent "x");
          Token (Equal);
          Token (Int 1);
        ];
      ];
    ]);

    make_parser_test
        "and/or operator precedence, parse tree"
        (*&& should be applied before ||, so &&
        node needs to be farther down parse tree*)
        "true || true && false"
        (Node [
            Token (Bool true);
            Token (Or);
            Node [
              Token (Bool true);
              Token (And);
              Token (Bool false)
            ];
          ]);

    make_parser_test
      "and/or/not operator precedence, parse tree"
      "not true || true && false"
      (Node [
          Node [
            Token (LowercaseIdent "not");
            Token (Bool true);
          ];
          Token (Or);
          Node [
            Token (Bool true);
            Token (And);
            Token (Bool false)
          ];
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
    [Expr (LetBinding (
      VarAssignment (
        ValueName "x",
        Constant (Int 1)
      ),
      InfixOp (
        VarName "x",
        Plus,
        Constant (Int 100)
      )
    ))];

  make_ast_converter_test
    "order of ops in infix expression"
    "let x = 1 in x - 100 - 200"
    [Expr (LetBinding (
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
      ))];

  make_ast_converter_test
    "order of ops in infix expression parentheiszed"
    "let x = 1 in (x - 100) - (200 - x)"
    [Expr (LetBinding (
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
        )))];

  make_ast_converter_test
    "order of ops in infix expression parentheiszed nested"
    "let x = 1 in (x - 100 - 300) - (200 - x)"
    [Expr (LetBinding (
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
        ))];

  make_ast_converter_test
    "function call"
    "let rec x a b c = a * b * c in x 1 2 3"
    [Expr (LetBinding (
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
    ))];

  make_ast_converter_test
    "greater than comparison expression"
    "1 > 2"
    [Expr (InfixOp (Constant (Int 1), GreaterThan, (Constant (Int 2))))];

  make_ast_converter_test
    "less than comparison expression"
    "1 < 2"
    [Expr (InfixOp (Constant (Int 1), LessThan, (Constant (Int 2))))];

  make_ast_converter_test
    "less than or equal comparison expression"
    "1 <= 2"
    [Expr (InfixOp (Constant (Int 1), LessThanOrEqual, (Constant (Int 2))))];

  make_ast_converter_test
    "greater than or equal comparison expression"
    "1 >= 2"
    [Expr (InfixOp (Constant (Int 1), GreaterThanOrEqual, (Constant (Int 2))))];

  make_ast_converter_test
    "is equal comparison expression"
    "1 = 2"
    [Expr (InfixOp (Constant (Int 1), Equal, (Constant (Int 2))))];

  make_ast_converter_test
    "not equal comparison expression"
    "1 <> 2"
    [Expr (InfixOp (Constant (Int 1), NotEqual, (Constant (Int 2))))];

  make_ast_converter_test
    "if then no else expression"
    "if 1 = 2 then 0"
    [Expr (Ternary (
      InfixOp (Constant (Int 1), Equal, (Constant (Int 2))),
      Constant (Int 0),
      None))];

  make_ast_converter_test
    "if then with else expression"
    "if 1 = 2 then 0 else 5"
    [Expr (Ternary (
      InfixOp (Constant (Int 1), Equal, (Constant (Int 2))),
      Constant (Int 0),
      Some (Constant (Int 5))))];

  make_ast_converter_test
    "anonymous function expression"
    "fun a b -> a + b"
    [Expr (Function (
      [ValueName "a"; ValueName "b"],
      InfixOp (
        VarName "a", Plus, VarName "b"
      ))
    )];

  make_ast_converter_test
    "semicolon expression"
    "1;2"
    [Expr (Sequential (Constant (Int 1), Constant (Int 2)))];

  make_ast_converter_test
    "times infix expression"
    "1 * 2"
    [Expr (InfixOp (Constant (Int 1), Times, (Constant (Int 2))))];

  make_ast_converter_test
    "divide infix expression"
    "1 / 2"
    [Expr (InfixOp (Constant (Int 1), Divide, (Constant (Int 2))))];

  make_ast_converter_test
    "negation expression"
    "~-1"
    [Expr (PrefixOp (Negation, Constant (Int 1)))];

  make_ast_converter_test
    "empty list"
    "[]"
    [Expr (Constant (EmptyList))];

  make_ast_converter_test
    "list literal, no trailing semicolon"
    "[1;2;3]"
    [Expr (ListExpr [
      Constant (Int 1);
      Constant (Int 2);
      Constant (Int 3);
    ])];

  make_ast_converter_test
    "list literal, trailing semicolon"
    "[1;2;3;]"
    [Expr (ListExpr [
      Constant (Int 1);
      Constant (Int 2);
      Constant (Int 3);
    ])];

  make_ast_converter_test
    "cons operator"
    "1::2::[]"
    [Expr (InfixOp (
      Constant (Int 1),
      Cons,
      InfixOp (
        Constant (Int 2),
        Cons,
        Constant (EmptyList)
      )
    ))];

  make_ast_converter_test
    "append operator"
    "[1;2;]@[3]"
    [Expr (InfixOp (
      ListExpr [
        Constant (Int 1);
        Constant (Int 2);
      ],
      Append,
      ListExpr [
        Constant (Int 3);
      ]
    ))];

  make_ast_converter_test
    "module accessor, list length of empty list"
    "List.length []"
    [Expr (FunctionCall (
      ModuleAccessor ("List", "length"),
      Constant (EmptyList)
    ))];

  make_ast_converter_test
    "module accessor, higher precedence than function call, higher than prefix"
    "List.create ~-List.empty_size"
    [Expr (FunctionCall (
      ModuleAccessor ("List", "create"),
      PrefixOp (
        Negation,
        ModuleAccessor ("List", "empty_size")
      )
    ))];

  make_ast_converter_test
    "let-rec-in fn (factorial) featuring if-else-then"
    "let rec fact x = if x=1 then 1 else x * fact(x-1) in fact 3"
    [Expr (LetBinding (
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
                    ))))))),
        FunctionCall(
          VarName "fact",
          Constant (Int 3)
        )
      ))];

  make_ast_converter_test
    "compilation module ast, single open decl"
    "open Test"
    [OpenDecl "Test"];

  make_ast_converter_test
    "compilation module ast, single let decl"
    "let x = 1"
    [LetDecl (VarAssignment (ValueName "x", Constant (Int 1)))];

  make_ast_converter_test
    "compilation module ast, single let rec decl"
    "let rec id x = x"
    [LetDecl (FunctionAssignment ("id", true, [ValueName "x"], VarName "x"))];

  make_ast_converter_test
    "compilation module ast, single let rec decl"
    "let rec id x = x"
    [LetDecl (FunctionAssignment ("id", true, [ValueName "x"], VarName "x"))];

  make_ast_converter_test
    "compilation module ast, double expr decl, start/end ;;"
    ";;print_int 1;;print_int 2;;"
    [
      Expr (FunctionCall (
        VarName "print_int",
        Constant (Int 1)
      ));
      Expr (FunctionCall (
        VarName "print_int",
        Constant (Int 2)
      ));
    ];

  make_ast_converter_test
    "compilation module ast, open decl, newline sep, let decl, ;; expr, ;; end"
    "open Pervasives\nlet x = 1;;print_int x;;"
    [
      OpenDecl "Pervasives";
      LetDecl (VarAssignment (
        ValueName "x",
        Constant (Int 1)
      ));
      Expr (FunctionCall (
        VarName "print_int",
        VarName "x"
      ));
    ];

  make_ast_converter_test
    "compilation module ast, multiple open decl, newline seps"
    "\n\nopen Pervasives\n\nopen List\nopen Pervasives"
    [
      OpenDecl "Pervasives";
      OpenDecl "List";
      OpenDecl "Pervasives";
    ];

  make_ast_converter_test
    "compilation module ast, open/let/open decl, newline seps"
    "\n\nopen Pervasives\n\nlet x = 1\nopen Pervasives"
    [
      OpenDecl "Pervasives";
      LetDecl (VarAssignment (
        ValueName "x",
        Constant (Int 1)
      ));
      OpenDecl "Pervasives";
    ];
]

let suite = "test suite"  >::: List.flatten [
  tokenizer_tests;
  parser_tests;
  ast_converter_tests;
]

let _ = run_test_tt_main suite
