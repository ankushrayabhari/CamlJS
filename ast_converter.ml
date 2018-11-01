open Token
open Parser
open Ast

let convert_prefix = function
  | Token.Negation -> Negation
  | Token.NegationFloat -> NegationFloat
  | t -> failwith (
      "prefix operator conversion not supported: " ^
      (Tokenizer.token_to_string t)
    )

let convert_infix = function
  | Token.Plus -> Plus
  | Token.PlusFloat -> PlusFloat
  | Token.Minus -> Minus
  | Token.MinusFloat -> MinusFloat
  | Token.Times -> Times
  | Token.TimesFloat -> TimesFloat
  | Token.Divide -> Divide
  | Token.DivideFloat -> DivideFloat
  | Token.GreaterThan -> GreaterThan
  | Token.LessThan -> LessThan
  | Token.GreaterThanOrEqual -> GreaterThanOrEqual
  | Token.LessThanOrEqual -> LessThanOrEqual
  | Token.NotEqual -> NotEqual
  | Token.Equal -> Equal
  | Token.Append -> Append
  | Token.Cons -> Cons
  | Token.Concat -> Concat
  | Token.And -> LogicalAnd
  | Token.Or -> LogicalOr
  | t -> failwith (
      "infix operator conversion not supported: " ^
      (Tokenizer.token_to_string t)
    )

let rec convert_pattern = function
  | Token (LowercaseIdent ident_name) -> ValueNamePattern (ident_name)
  | Token (Ignore) -> IgnorePattern
  | Token (Int v) -> ConstantPattern (Int v)
  | Token (Float v) -> ConstantPattern (Float v)
  | Token (CharLiteral v) -> ConstantPattern (CharLiteral v)
  | Token (StringLiteral v) -> ConstantPattern (StringLiteral v)
  | Token (EmptyList) -> ConstantPattern (EmptyList)
  | Token (Bool b) -> ConstantPattern (Bool b)
  | Token (Unit) -> ConstantPattern (Unit)
  | Node [
      pat;
      Token (Token.As);
      Token (LowercaseIdent alias)
    ] -> AliasPattern (convert_pattern pat, alias)
  | Node [
      paren_pat;
      Token (Cons);
      cons_pat;
    ] -> ConsPattern (convert_pattern paren_pat, convert_pattern cons_pat)
  | Node [
      Token (LParen);
      pat;
      Token (RParen);
    ] -> ParenPattern (convert_pattern pat)
  | Node [
      Token (StartList);
      patterns_semicolon_sep;
      Token (SemiColon);
      Token (EndList);
    ]
  | Node [
      Token (StartList);
      patterns_semicolon_sep;
      Token (EndList);
    ] -> ListPattern (
        convert_one_or_more_patterns_semicolon_sep patterns_semicolon_sep
        |> List.rev
      )
  | _ -> failwith "not a valid pattern"

and convert_one_or_more_patterns_semicolon_sep = function
  | Node [
      patterns_semicolon_sep;
      Token (SemiColon);
      pat;
    ] ->
      convert_pattern pat::
      convert_one_or_more_patterns_semicolon_sep patterns_semicolon_sep
  | pat -> [convert_pattern pat]

let rec get_patterns one_or_more_patterns acc =
  try convert_pattern one_or_more_patterns::acc
  with _ ->
    match one_or_more_patterns with
    | Node [params_ptree; pat] ->
      get_patterns params_ptree ((convert_pattern pat)::acc)
    | _ -> failwith "not a valid one or more patterns"

let rec convert_let_binding is_rec = function
  | Node [
      Token(Token.LowercaseIdent f_name);
      one_or_more_pattern;
      Token(Token.Equal);
      let_expr;
    ] ->
      FunctionAssignment (
        f_name,
        is_rec,
        get_patterns one_or_more_pattern [],
        convert_expr let_expr
      )

  | Node [
      bound_to_pattern;
      Token (Token.Equal);
      let_expr;
    ] ->
      VarAssignment (
        convert_pattern bound_to_pattern,
        convert_expr let_expr
      )
  | _ -> failwith "not a valid convert let binding"

and convert_one_or_more_if_expr acc = function
  | Node [
      one_or_more_expr;
      Token (Token.SemiColon);
      expr;
    ] -> convert_one_or_more_if_expr ((convert_expr expr)::acc) one_or_more_expr
  | expr -> (convert_expr expr)::acc

and convert_pattern_matching acc = function
  | Node [
      Token (VerticalBar);
      pat;
      Token (FunctionArrow);
      value_expr;
    ]
  | Node [
      pat;
      Token (FunctionArrow);
      value_expr;
    ] ->
    (convert_pattern pat, convert_expr value_expr, None)::acc
  | Node [
      pat;
      Token (FunctionArrow);
      value_expr;
      further_pattern_matching;
    ]
  | Node [
      Token (VerticalBar);
      pat;
      Token (FunctionArrow);
      value_expr;
      further_pattern_matching;
    ] ->
      convert_pattern_matching
        ((convert_pattern pat, convert_expr value_expr, None)::acc)
        further_pattern_matching
  | _ -> failwith "not a valid pattern matching"

and convert_expr tr = match tr with
  | Token (Token.Int v) -> Constant (Int v)

  | Token (Token.Bool b) -> Constant (Bool b)

  | Token (Token.Float v) -> Constant (Float v)

  | Token (Token.CharLiteral v) -> Constant (CharLiteral v)

  | Token (Token.StringLiteral v) -> Constant (StringLiteral v)

  | Token (Token.EmptyList) -> Constant (EmptyList)

  | Token (Token.LowercaseIdent n) -> VarName n

  | Token (Token.Unit) -> Constant (Unit)

  | Node [
      Token (CapitalizedIdent module_name);
      Token (Period);
      Token (LowercaseIdent value_name);
    ] ->
      ModuleAccessor (module_name, value_name)

  | Node [
      Token (Token.LParen);
      expr;
      Token (Token.RParen);
    ] ->
      ParenExpr (convert_expr expr)

  | Node [
      Token (pre);
      expr;
    ] when Tokenizer.has_tag pre "prefix" ->
      PrefixOp (convert_prefix pre, convert_expr expr)

  | Node [
      expr1;
      Token (infix);
      expr2;
    ] when Tokenizer.has_tag infix "infix" ->
      InfixOp (convert_expr expr1, convert_infix infix, convert_expr expr2)

  | Node [
      Token (Token.If);
      cond_expr;
      Token (Token.Then);
      then_expr;
    ] ->
      Ternary (convert_expr cond_expr, convert_expr then_expr, None)

  | Node [
      Token (Token.If);
      cond_expr;
      Token (Token.Then);
      then_expr;
      Token (Token.Else);
      else_expr;
    ] ->
      Ternary (
        convert_expr cond_expr,
        convert_expr then_expr,
        Some (convert_expr else_expr)
      )

  | Node [
      Token(Token.Fun);
      one_or_more_patterns;
      Token(Token.FunctionArrow);
      anon_func_expr;
    ] ->
      Function (
        get_patterns one_or_more_patterns [],
        convert_expr anon_func_expr
      )

  | Node [
      expr1;
      Token (Token.SemiColon);
      expr2;
    ] ->
      Sequential (convert_expr expr1, convert_expr expr2)

  | Node [
      Token(Token.Let);
      Token(Token.Rec);
      let_binding;
      Token(Token.In);
      let_expr;
    ] -> LetBinding (
      convert_let_binding true let_binding,
      convert_expr let_expr
    )

  | Node [
      Token(Token.Let);
      let_binding;
      Token (Token.In);
      let_expr;
    ] ->
      LetBinding (convert_let_binding false let_binding, convert_expr let_expr)

  | Node [
      fun_expr;
      arg_expr;
    ] ->
      FunctionCall (convert_expr fun_expr, convert_expr arg_expr)

  | Node [
      Token (Token.StartList);
      one_or_more_expr;
      Token (Token.EndList);
    ]
  | Node [
      Token (Token.StartList);
      one_or_more_expr;
      Token (Token.SemiColon);
      Token (Token.EndList);
    ] ->
      ListExpr (convert_one_or_more_if_expr [] one_or_more_expr)

  | Node [
      Token (Match);
      target_expr;
      Token (With);
      pattern_matching;
    ] ->
      MatchExpr (
        convert_expr target_expr,
        convert_pattern_matching [] pattern_matching |> List.rev
      )

  | _ -> failwith "not a valid expression"

let convert_definition = function
  | Node [
      Token (Open);
      Token (CapitalizedIdent module_name);
    ] ->
      OpenDecl module_name
  | Node [
      Token(Token.Let);
      Token(Token.Rec);
      let_binding;
    ] ->
      LetDecl (convert_let_binding true let_binding)
  | Node [
      Token(Token.Let);
      let_binding;
    ] ->
      LetDecl (convert_let_binding false let_binding)
  | _ -> failwith "not a valid definition"


(**
 * [convert_module_item tr] is the expression or declaration of module [tr], 
 * where [tr] corresponds to some module.
 * Requires:
 * - [tr] is a valid module expression or declaration.
*)
let convert_module_item tr =
  try Expr (convert_expr tr) with _ -> (convert_definition tr)

(**
 * [convert_one_plus_def_expr tr] is the list of declarations and expressions
 * of module [tr].
 * Requires:
 * - [tr] is a valid module expression or declaration.
*)
let rec convert_one_plus_def_expr = function
  | Node [
      Token (DoubleSemicolon);
      tr;
      def_expr_tr;
    ] -> (convert_module_item tr)::(convert_one_plus_def_expr def_expr_tr)
  | Node [
      Token (DoubleSemicolon);
      tr;
    ] -> [convert_module_item tr]
  | tr -> try [convert_definition tr] with _ ->
      match tr with
      | Node [
          tr;
          def_expr_tr;
        ] -> (convert_module_item tr)::(convert_one_plus_def_expr def_expr_tr)
      | _ -> failwith "not a valid expr def no start ;;"

(**
 * [convert_expr_definition_no_start_double_semicolon tr] is the list of 
 * declarations and expressions of module [tr].
 * Requires:
 * - [tr] is a valid module expression or declaration.
*)
let convert_expr_definition_no_start_double_semicolon = function
  | Node [
      tr;
      def_expr_tr;
      Token (DoubleSemicolon);
    ] -> (convert_module_item tr)::(convert_one_plus_def_expr def_expr_tr)
  | Node [
      tr;
      Token (DoubleSemicolon);
    ] -> [convert_module_item tr]
  | tr -> try [convert_module_item tr] with _ ->
    match tr with
      | Node [
          tr;
          def_expr_tr;
        ] -> (convert_module_item tr)::(convert_one_plus_def_expr def_expr_tr)
      | _ -> failwith "not a valid expr def no start ;;"

(**
 * [convert_ast tr] is the list of declarations and expressions of module [tr].
 * Requires:
 * - [tr] is a valid module expression or declaration.
*)
let convert_ast = function
  | Node [
      Token (DoubleSemicolon);
      tr;
    ]
  | tr -> convert_expr_definition_no_start_double_semicolon tr

(**
 * [convert parse_tr] is the ast conversion of the declarations and expressions
 * of module [tr].
 * Requires:
 * - [tr] is a valid module expression or declaration.
*)
let convert parse_tr =
  convert_ast parse_tr
