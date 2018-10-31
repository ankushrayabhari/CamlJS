open OUnit2

let make_tokenizer_test name program expected_value =
  name >:: (fun _ ->
    let tokenized_program = try Tokenizer.tokenize program with _ -> [||] in
    let expected_tokens = expected_value |> Array.of_list in
    assert_equal expected_tokens tokenized_program
  )

let make_parser_test name program expected_tree =
  name >:: (fun _ ->
    let parse_tr = Tokenizer.tokenize program |> Parser.parse in
    assert_equal expected_tree parse_tr
  )

let make_ast_converter_test name program expected_tree =
  name >:: (fun _ ->
    let ast =
      Tokenizer.tokenize program
      |> Parser.parse
      |> Ast_converter.convert
    in
    assert_equal expected_tree ast
  )

let tokenizer_tests = Token.[
  make_tokenizer_test
    "concatenate two strings"
    "\"a\" ^ \"b\""
    [StringLiteral "\"a\""; Concat; StringLiteral "\"b\""];

  make_tokenizer_test
    "empty string"
    "\"\""
    [StringLiteral "\"\""];

  make_tokenizer_test
    "string a"
    "\"a\""
    [StringLiteral "\"a\""];

  make_tokenizer_test
    "two strings"
    "\"a\"     \"b\""
    [StringLiteral "\"a\""; StringLiteral "\"b\""];

  make_tokenizer_test
    "string newline char"
    "\"a\\n\""
    [StringLiteral "\"a\\n\""];

  make_tokenizer_test
    "string abc"
    "\"abc\""
    [StringLiteral "\"abc\""];

  make_tokenizer_test
    "char a"
    "'a'"
    [CharLiteral "'a'"];

  make_tokenizer_test
    "char \\n"
    "'\\n'"
    [CharLiteral "'\\n'"];

  make_tokenizer_test
    "char \\r"
    "'\r'"
    [];

  make_tokenizer_test
    "char \\t"
    "'\t'"
    [];

  make_tokenizer_test
    "char \\n"
    "'\\n'"
    [CharLiteral "'\\n'"];

  make_tokenizer_test
    "char \\r"
    "'\b'"
    [];

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
    [Let; LowercaseIdent "x"; Equal; Bool true; In; Unit];

  make_tokenizer_test
    "and test, tokenizer"
    "let x = (true && false)"
    [Let; LowercaseIdent "x"; Equal; LParen; Bool true; And; Bool false;
     RParen];

  make_tokenizer_test
    "or test, tokenizer"
    "let bool_expr = true || false in \nlet x = 1"
    [Let; LowercaseIdent "bool_expr"; Equal; Bool true; Or; Bool false; In;
     Let; LowercaseIdent "x"; Equal; Int 1];

  make_tokenizer_test
    "not test, tokenizer"
    "let x = not false"
    [Let; LowercaseIdent "x"; Equal; LowercaseIdent "not"; Bool false];

  make_tokenizer_test
    "complicated and/or/not test, tokenizer"
    "let x = not (true && false)"
    [Let; LowercaseIdent "x"; Equal; LowercaseIdent "not"; LParen; Bool true;
     And; Bool false; RParen];

  make_tokenizer_test
    "pattern match expression"
    "match x with | 1 when x = 1 -> true | _ -> false"
    [Match; LowercaseIdent "x"; With; VerticalBar; Int 1; When;
     LowercaseIdent "x"; Equal; Int 1; FunctionArrow; Bool true; VerticalBar;
     Ignore; FunctionArrow; Bool false;];

  make_tokenizer_test
    "pattern match expression in a let assignment"
    "let _ = 'c'"
    [Let; Ignore; Equal; CharLiteral "'c'";];

  make_tokenizer_test
    "pattern match expression in a let assignment"
    "match x with _ as n"
    [Match; LowercaseIdent "x"; With; Ignore; As; LowercaseIdent "n";];

  make_tokenizer_test
    "simple unit test, tokenizer"
    "let x = ()"
    [Let; LowercaseIdent "x"; Equal; Unit];

  make_tokenizer_test
    "unit test 2, tokenizer"
    "let () = print_string \"hi\""
    [Let; Unit; Equal; LowercaseIdent "print_string"; StringLiteral ("\"hi\"")];
]

let parser_tests = Parser.(Tokenizer.[
  make_parser_test
    "concat 2 strings"
    "\"a\" ^ \"b\";;"
    (Node [
      Node [
        Token (StringLiteral "\"a\"");
        Token (Concat);
        Token (StringLiteral "\"b\"");
      ];
      Token (DoubleSemicolon);
    ]);


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
    "list literal, precedence over function calls with float negation"
    "f [~-.1.; 0.00]"
    (Node [
      Token (LowercaseIdent "f");
      Node [
        Token (StartList);
        Node [
          Node [
            Token (NegationFloat);
            Token (Float 1.);
          ];
          Token (SemiColon);
          Token (Float 0.);
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
    "compilation unit, start with ;;, single expr"
    ";;1.+.1."
    (Node [
      Token (DoubleSemicolon);
      Node [
        Token (Float 1.);
        Token (PlusFloat);
        Token (Float 1.);
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
    "Float operations"
    "1.6/.2.+.3.*.05.2"
    (Node [
      (Node [
        Token (Float 1.6);
        Token DivideFloat;
        Token (Float 2.)
      ]);
      Token PlusFloat;
      (Node [
        Token (Float 3.);
        Token TimesFloat;
        Token (Float 5.2)
      ])
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
    "compilation unit, start/end with ;;, double definition"
    ";;let x = 1.;;let rec x = 01.0;;"
    (Node [
      Token (DoubleSemicolon);
      Node [
        Node [
          Token (Let);
          Node [
            Token (LowercaseIdent "x");
            Token (Equal);
            Token (Float 1.);
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
              Token (Float 1.);
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

  make_parser_test
    "and operator associativity, parse tree"
    "true && true && false"
    (Node [
        Token (Bool true);
        Token (And);
        Node [
          Token (Bool true);
          Token (And);
          Token (Bool false)
        ];
      ]);

  make_parser_test
    "or operator associativity, parse tree"
    "true || true || false"
    (Node [
        Token (Bool true);
        Token (Or);
        Node [
          Token (Bool true);
          Token (Or);
          Token (Bool false)
        ];
      ]);

  make_parser_test
    "Simple Unit test, parse tree"
    "let x = ()"
    (Node [
        Token (Let);
        Node[
          Token (LowercaseIdent "x");
          Token (Equal);
          Token (Unit);
        ];
      ]);

  make_parser_test
    "pattern matching precedence"
    "let (1)::[2;_;] as x = []"
    (Node [
      Token (Let);
      Node [
        Node [
          Node [
            Node [
              Token (LParen);
              Token (Int 1);
              Token (RParen);
            ];
            Token (Cons);
            Node [
              Token (StartList);
              Node [
                Token (Int 2);
                Token (SemiColon);
                Token (Ignore);
              ];
              Token (SemiColon);
              Token (EndList);
            ];
          ];
          Token (As);
          Token (LowercaseIdent "x");
        ];
        Token (Equal);
        Token (EmptyList);
      ];
    ]);

  make_parser_test
    "pattern matching cons associativity"
    "let 1::2::3 = []"
    (Node [
      Token (Let);
      Node [
        Node [
          Token (Int 1);
          Token (Cons);
          Node [
            Token (Int 2);
            Token (Cons);
            Token (Int 3);
          ];
        ];
        Token (Equal);
        Token (EmptyList);
      ];
    ]);

  make_parser_test
    "match expression on lists"
    "match [] with h::t -> true | [] -> false"
    (Node [
      Token (Match);
      Token (EmptyList);
      Token (With);
      Node [
        Node [
          Token (LowercaseIdent "h");
          Token (Cons);
          Token (LowercaseIdent "t");
        ];
        Token (FunctionArrow);
        Token (Bool true);
        Node [
          Token (VerticalBar);
          Token (EmptyList);
          Token (FunctionArrow);
          Token (Bool false);
        ];
      ];
    ]);

  make_parser_test
    "match expression on floats, starting cases with vertical bar"
    "match 1.0 with | 1.5 -> true | _ -> false"
    (Node [
      Token (Match);
      Token (Float 1.0);
      Token (With);
      Node [
        Token (VerticalBar);
        Token (Float 1.5);
        Token (FunctionArrow);
        Token (Bool true);
        Node [
          Token (VerticalBar);
          Token (Ignore);
          Token (FunctionArrow);
          Token (Bool false);
        ];
      ];
    ]);

  make_parser_test
    "match expressions nesting, starting cases with vertical bar"
    {|
      match match 1 with | 1 -> true | 0 -> false with
      true -> match 0 with 0 -> false
    |}
    (Node [
      Token (Match);
      Node [
        Token (Match);
        Token (Int 1);
        Token (With);
        Node [
          Token (VerticalBar);
          Token (Int 1);
          Token (FunctionArrow);
          Token (Bool true);
          Node [
            Token (VerticalBar);
            Token (Int 0);
            Token (FunctionArrow);
            Token (Bool false);
          ];
        ];
      ];
      Token (With);
      Node [
        Token (Bool true);
        Token (FunctionArrow);
        Node [
          Token (Match);
          Token (Int 0);
          Token (With);
          Node [
            Token (Int 0);
            Token (FunctionArrow);
            Token (Bool false);
          ];
        ];
      ];
    ]);
])

let ast_converter_tests = Ast.[
  make_ast_converter_test
    "let concat 2 strings test"
    "let x = \"a\" in x ^ \"b\""
    [Expr (LetBinding (
      VarAssignment (
        ValueNamePattern "x",
        Constant (StringLiteral "\"a\"")
      ),
      InfixOp (
        VarName "x",
        Concat,
        Constant (StringLiteral "\"b\"")
      )
    ))];

  make_ast_converter_test
    "basic_test"
    "let x = 1 in x + 100"
    [Expr (LetBinding (
      VarAssignment (
        ValueNamePattern "x",
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
          ValueNamePattern "x",
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
    "order of ops in infix expression with floats"
    "let x = 1. in x -. 100. -. 200."
    [Expr (LetBinding (
        VarAssignment (
          ValueNamePattern "x",
          Constant (Float 1.)
        ),
        InfixOp (
          InfixOp (
            VarName "x",
            MinusFloat,
            Constant (Float 100.)
          ),
          MinusFloat,
          Constant (Float 200.)
        )
      ))];

  make_ast_converter_test
    "order of ops in infix expression parentheiszed"
    "let x = 1 in (x - 100) - (200 - x)"
    [Expr (LetBinding (
        VarAssignment (
          ValueNamePattern "x",
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
            ValueNamePattern "x",
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
          ValueNamePattern "a";
          ValueNamePattern "b";
          ValueNamePattern "c";
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
    "function call floats"
    "let rec x a b c = a *. b *. c in x 1. 2. 3."
    [Expr (LetBinding (
      FunctionAssignment (
        "x",
        true,
        [
          ValueNamePattern "a";
          ValueNamePattern "b";
          ValueNamePattern "c";
        ],
        InfixOp (
          InfixOp (
            VarName "a",
            TimesFloat,
            VarName "b"
          ),
          TimesFloat,
          VarName "c"
        )
      ),
      FunctionCall (
        FunctionCall (
          FunctionCall (
            VarName "x",
            Constant (Float 1.)
          ),
          Constant (Float 2.)
        ),
        Constant (Float 3.)
      )
    ))];

  make_ast_converter_test
    "float operations"
    "1.6/.2.+.3.*.05.2"
    [Expr
    (InfixOp
      (InfixOp (Constant (Float 1.6), DivideFloat, Constant (Float 2.)),
      PlusFloat,
      InfixOp (Constant (Float 3.), TimesFloat, Constant (Float 5.2))))
    ];

  make_ast_converter_test
    "greater than comparison expression"
    "1 > 2"
    [Expr (InfixOp (Constant (Int 1), GreaterThan, (Constant (Int 2))))];

  make_ast_converter_test
    "greater than comparison expression floats"
    "1. > 2."
    [Expr (InfixOp (Constant (Float 1.), GreaterThan, (Constant (Float 2.))))];

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
      [ValueNamePattern "a"; ValueNamePattern "b"],
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
          [ValueNamePattern "x";],
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
    [LetDecl (VarAssignment (ValueNamePattern "x", Constant (Int 1)))];

  make_ast_converter_test
    "compilation module ast, single let rec decl"
    "let rec id x = x"
    [LetDecl (FunctionAssignment (
      "id",
      true,
      [ValueNamePattern "x"],
      VarName "x"
    ))];

  make_ast_converter_test
    "compilation module ast, single let rec decl"
    "let rec id x = x"
    [LetDecl (FunctionAssignment (
      "id",
      true,
      [ValueNamePattern "x"],
      VarName "x"
    ))];

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
        ValueNamePattern "x",
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
        ValueNamePattern "x",
        Constant (Int 1)
      ));
      OpenDecl "Pervasives";
    ];

  make_ast_converter_test
    "char literal with functioncall test"
    "\n\nopen Char;;print_int (Char.code 'c')"
    [
      OpenDecl "Char";
      Expr (FunctionCall (
        VarName "print_int",
        ParenExpr (FunctionCall (
          ModuleAccessor ("Char", "code"),
          Constant (CharLiteral "'c'")
        )
      )));
    ];

  make_ast_converter_test
    "let declaration, pattern matching against list with cons"
    "let 1::x = [1;2;3;4]"
    [LetDecl (VarAssignment (
      ConsPattern (
        ConstantPattern (Int 1),
        ValueNamePattern "x"
      ),
      ListExpr [
        Constant (Int 1);
        Constant (Int 2);
        Constant (Int 3);
        Constant (Int 4);
      ]
    ))];

  make_ast_converter_test
    "let declaration, pattern matching against list with list"
    "let [1;x;_;_] = [1;2;3;4]"
    [LetDecl (VarAssignment (
      ListPattern [
        ConstantPattern (Int 1);
        ValueNamePattern "x";
        IgnorePattern;
        IgnorePattern;
      ],
      ListExpr [
        Constant (Int 1);
        Constant (Int 2);
        Constant (Int 3);
        Constant (Int 4);
      ]
    ))];

  make_ast_converter_test
    "let declaration, pattern matching against constant"
    "let 1 = 1"
    [LetDecl (VarAssignment (
      ConstantPattern (Int 1),
      Constant (Int 1)
    ))];

  make_ast_converter_test
    "let declaration, pattern matching against ignore"
    "let _ = 'a'"
    [LetDecl (VarAssignment (
      IgnorePattern,
      Constant (CharLiteral "'a'")
    ))];

  make_ast_converter_test
    "let declaration, pattern matching against paren pattern"
    "let (x) = \"asdfasdf\""
    [LetDecl (VarAssignment (
      ParenPattern (ValueNamePattern "x"),
      Constant (StringLiteral "\"asdfasdf\"")
    ))];

  make_ast_converter_test
    "let fun declaration, pattern matching against list with cons"
    "let rec f (x::1::[]) y = [1;2;3;4]"
    [LetDecl (FunctionAssignment (
      "f", true,
      [
        ParenPattern (ConsPattern (
          ValueNamePattern "x",
          ConsPattern (
            ConstantPattern (Int 1),
            ConstantPattern (EmptyList)
          )
        ));
        ValueNamePattern "y";
      ],
      ListExpr [
        Constant (Int 1);
        Constant (Int 2);
        Constant (Int 3);
        Constant (Int 4);
      ]
    ))];

  make_ast_converter_test
    "fun declaration, pattern matching against list with list"
    "fun [1;x;_;_] -> \"qwertyuiop\""
    [Expr (Function (
      [ListPattern [
        ConstantPattern (Int 1);
        ValueNamePattern "x";
        IgnorePattern;
        IgnorePattern;
      ]],
      Constant (StringLiteral "\"qwertyuiop\"")
    ))];

  make_ast_converter_test
    "fun declaration, pattern matching against constant"
    "fun 1 -> 1.0"
    [Expr (Function (
      [ConstantPattern (Int 1)],
      Constant (Float 1.0)
    ))];

  make_ast_converter_test
    "let fun declaration, pattern matching against ignore"
    "let f _ = '\\n'"
    [LetDecl (FunctionAssignment (
      "f", false,
      [IgnorePattern],
      Constant (CharLiteral "'\\n'")
    ))];

  make_ast_converter_test
    "let fun declaration, pattern matching against alias"
    "let rec yolo (1 as x) = \"asdfasdf\""
    [LetDecl (FunctionAssignment (
      "yolo", true,
      [ParenPattern (AliasPattern (ConstantPattern (Int 1), "x"))],
      Constant (StringLiteral "\"asdfasdf\"")
    ))];

  make_ast_converter_test
    "pattern match on list with list literal, no guard"
    "match [] with | [1;2;3] -> 0"
    [Expr (MatchExpr (
      Constant (EmptyList),
      [(
        ListPattern [
          ConstantPattern (Int 1);
          ConstantPattern (Int 2);
          ConstantPattern (Int 3);
        ],
        Constant (Int 0),
        None
      )]
    ))];

  make_ast_converter_test
    "pattern match on list with cons expr, no guard"
    "match [1;2;3] with h::t -> t"
    [Expr (MatchExpr (
      ListExpr [Constant (Int 1); Constant (Int 2); Constant (Int 3)],
      [(
        ConsPattern (
          ValueNamePattern "h",
          ValueNamePattern "t"
        ),
        VarName "t",
        None
      )]
    ))];

  make_ast_converter_test
    "pattern match on list with cons, ignore and alias, no guard"
    "match [1;2;3] with _::_ as t -> t"
    [Expr (MatchExpr (
      ListExpr [Constant (Int 1); Constant (Int 2); Constant (Int 3)],
      [(
        AliasPattern (
          ConsPattern (
            IgnorePattern,
            IgnorePattern
          ),
          "t"
        ),
        VarName "t",
        None
      )]
    ))];

  make_ast_converter_test
    "pattern match on int with paren, ignore, alias, no guard"
    "match 1 with (_) as t -> t"
    [Expr (MatchExpr (
      Constant (Int 1),
      [(
        AliasPattern (
          ParenPattern (
            IgnorePattern
          ),
          "t"
        ),
        VarName "t",
        None
      )]
    ))];

  make_ast_converter_test
    "nested pattern matcing on int, float, no guard"
    "match match 1 with | 1 -> 1.0 | 0 -> 2.0 with 0.0 -> match 0 with 0 -> 'a'"
    [Expr (MatchExpr (
      MatchExpr (
        Constant (Int 1),
        [
          (ConstantPattern (Int 1), Constant (Float 1.0), None);
          (ConstantPattern (Int 0), Constant (Float 2.0), None);
        ]
      ),
      [(
        ConstantPattern (Float 0.0), MatchExpr (Constant (Int 0), [
          (ConstantPattern (Int 0), Constant (CharLiteral "'a'"), None)
        ]), None
      )]
    ))];

  make_ast_converter_test
    "simple Unit test, ast conversion"
    "let x = ()"
    [
      LetDecl (VarAssignment (
        ValueNamePattern "x",
        Constant (Unit)
      ));
    ];
]

let suite = "test suite"  >::: List.flatten [
  tokenizer_tests;
  parser_tests;
  ast_converter_tests;
]

let _ = run_test_tt_main suite
