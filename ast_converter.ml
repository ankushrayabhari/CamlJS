(** Conveter from Parse Tree to AST *)
open Token
open Parser
open Ast

(**
 * [convert_prefix tok] is the AST-representation of the prefix token, [tok].
 *
 * {b Requires}:
 * - [tok] corresponds to a valid prefix operator
 * - [tok] is a Token leaf
 *)
let convert_prefix = function
  | Token.Negation -> Negation
  | Token.NegationFloat -> NegationFloat
  | t -> failwith (
      "prefix operator conversion not supported: " ^
      (Tokenizer.token_to_string t)
    )

(**
 * [convert_infix tok] is the AST-representation of the infix token, [tok].
 *
 * {b Requires}:
 * - [tok] corresponds to a valid infix operation
 * - [tok] is a Token leaf
 *)
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

(**
 * [convert_pattern tr] is the AST-representation of the pattern
 * production of [tr].
 *
 * {b Requires}:
 * - [tr] corresponds to a valid pattern production according to [grammar.json].
 *
 * {b Raises}:
 * - [Failure] if [tr] does not correspond to a valid pattern tree.
 *)
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

(**
 * [convert_one_or_more_patterns_semicolon_sep tree] is the AST-representation
 * of [tree] where [tree] must either be:
 * - directly convertible by [convert_pattern]
 * - has a pattern semicolon sep-legal left child, a Token (Semicolon) as a
 * middle child, and a pattern-legal right child.
 *
 * {b Raises}:
 * - [Failure] if [tr] does not correspond to a valid pattern semicolon sep
 * tree.
 *)
and convert_one_or_more_patterns_semicolon_sep = function
  | Node [
      patterns_semicolon_sep;
      Token (SemiColon);
      pat;
    ] ->
      convert_pattern pat::
      convert_one_or_more_patterns_semicolon_sep patterns_semicolon_sep
  | pat -> [convert_pattern pat]

(**
 * [get_patterns one_or_more_patterns acc] is the patterns in
 * [one_or_more_patterns] appended to [acc].
 *
 * {b Requires}:
 * - one_or_more_pattern is a valid pattern
 * - one_or_more_pattern is a Node where the first element is another
 * one or more patterns tree and the second node is a valid pattern.
 *
 * {b Raises}:
 * - [Failure] if it is not a one or more patterns
 *)
let rec get_patterns one_or_more_patterns acc =
  try convert_pattern one_or_more_patterns::acc
  with _ ->
    match one_or_more_patterns with
    | Node [params_ptree; pat] ->
      get_patterns params_ptree ((convert_pattern pat)::acc)
    | _ -> failwith "not a valid one or more patterns"

(**
 * [convert_let_binding is_rec t] is the abstract syntax tree representing
 * the let binding represented by parse_tree [t].
 *
 * [is_rec] is true if the let binding is recursive and false otherwise.
 *
 * {b Raises}: Failure if [t] does not represent a type of let binding.
 *)
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

(** [convert_list_body_expr acc t] is the list of abstract syntax trees of
 * type expression in the parse_tree [t], where all expressions are seperated
 * by a semicolon.
 *
 * {b Requires}: [t] only has expressions of precedence equal to or higher than
 * an if expression, and all expressions are separated by a semicolon.
 *
 * {b Example}:
 * [convert_list_body_expr
 *   []
 *   (Node [
 *     Token (StartList);
 *     Node [
 *       Node [
 *         Token (Int 1);
 *         Token (SemiColon);
 *         Token (Int 2);
 *       ];
 *       Token (SemiColon);
 *       Token (Int 3);
 *     ];
 *     Token (SemiColon);
 *     Token (EndList);
 *   ])
 * ]
 * gives the AST list:
 * [Expr Constant (Int 1), Expr Constant (Int 2), Expr Constant (Int 3)].
 *)
and convert_list_body_expr acc = function
  | Node [
      one_or_more_expr;
      Token (Token.SemiColon);
      expr;
    ] -> convert_list_body_expr ((convert_expr expr)::acc) one_or_more_expr
  | expr -> (convert_expr expr)::acc

(**
 * [convert_pattern_matching acc t] is the abstract syntax tree list of
 * tuples (pattern, value, Some guard) representing the parse_tree [t] that
 * represents a pattern matching with each pattern match being
 * Node [pat; Token (FunctionArrow); val].
 *
 * See that [pattern] is the AST
 * representation of [pat], and [value] is the AST
 * representation of [val].
 *
 * Raises: Failure if [t] is not a parse_tree representing pattern matching.
 *)
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

(**
 * [convert_expr tr] is the abstract syntax tree representing the parse_tree
 * [tr].
 *
 * Raises: Failure if [t] is not a parse_tree reprensenting an expr.
 *)
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
      ListExpr (convert_list_body_expr [] one_or_more_expr)

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

(**
 * [convert_definition t] is the abstract syntax tree representing the
 * parse_tree [t] which represents a definition (an [Open Module] or
 * [let var_name = expr]).
 *
 * Raises: Failure if [t] does not represent a valid definition.
 *)
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

let convert_module_item tr =
  try Expr (convert_expr tr) with _ -> (convert_definition tr)

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

let convert_ast = function
  | Node [
      Token (DoubleSemicolon);
      tr;
    ]
  | tr -> convert_expr_definition_no_start_double_semicolon tr

let convert parse_tr =
  convert_ast parse_tr
