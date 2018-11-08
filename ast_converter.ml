open Token
open Parser
open Parse_tree
open Ast

(**
 * [convert_prefix tok] is the AST-representation of the prefix token, [tok].
 * @raise Failure if [tok] does not correspond to a valid prefix operator
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
 * @raise Failure if [tok] does not correspond to a valid infix operator
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
 * production of the parse tree [tr].
 *
 * @raise Failure if [tr] does not correspond to one of the following
 * structures:
 * - [tr] is a Token representing an identifier or constant.
 * - [tr] is an alias pattern parse tree with the elements of [Node] being
 * {ol
 * {li the pattern}
 * {li the [As] token}
 * {li the [LowercaseIdent] token that binds to the value of the whole pattern}
 * }
 * - [tr] is a cons pattern parse tree with the elements of [Node] being
 * {ol
 * {li the head of list pattern}
 * {li the [Cons] token}
 * {li the pattern for the tail of the list}
 * }
 * - [tr] is a paren pattern parse tree with the elements of [Node] being
 * {ol
 * {li a [LParen] token}
 * {li the pattern}
 * {li  a [RParen] token}
 * }
 * - [tr] is a list pattern parse tree with the elements of [Node] being
 * {ol
 * {li a [StartList] token}
 * {li a valid semicolon separated pattern parse see. {b See:}
 * [convert_one_or_more_patterns_semicolon_sep]}
 * {li an optional [SemiColon] token}
 * {li a [EndList] token}
 * }
 *)
let rec convert_pattern = function
  | Token (LowercaseIdent ident_name) -> ValueNamePattern (ident_name)
  | Token (Ignore) -> IgnorePattern
  | Token (Int v) -> ConstantPattern (Int v)
  | Token (Float v) -> ConstantPattern (Float v)
  | Token (CharLiteral v) -> ConstantPattern (CharLiteral v)
  | Token (StringLiteral v) -> ConstantPattern (StringLiteral v)
  | Token (EmptyList) -> ConstantPattern (EmptyList)
  | Token (EmptyArray) -> ConstantPattern (EmptyArray)
  | Token (Bool b) -> ConstantPattern (Bool b)
  | Token (Unit) -> ConstantPattern (Unit)
  | Node [
      Token (CharLiteral s);
      Token (DoublePeriod);
      Token (CharLiteral e)
    ] -> RangedCharacterPattern (s, e)
  | Node [
      pat;
      Token (Token.As);
      Token (LowercaseIdent alias)
    ] -> AliasPattern (convert_pattern pat, alias)
  | Node [
      pat;
      Token (Comma);
      one_or_more_pat;
    ] -> TuplePattern(
      [convert_pattern pat] @
      List.rev (convert_one_or_more_patterns_comma_sep one_or_more_pat)
      )
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
  | Token (CapitalizedIdent constr) -> VariantPattern (constr, None)
  | Node [
      Token (CapitalizedIdent constr);
      pat
    ] -> VariantPattern (constr, Some (convert_pattern pat))
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
  | Node [
      Token (StartArray);
      patterns_semicolon_sep;
      Token (SemiColon);
      Token (EndArray);
    ]
  | Node [
      Token (StartArray);
      patterns_semicolon_sep;
      Token (EndArray);
    ] -> ArrayPattern (
        convert_one_or_more_patterns_semicolon_sep patterns_semicolon_sep
        |> List.rev
      )
  | _ -> failwith "not a valid pattern"

(**
 * [convert_one_or_more_patterns_semicolon_sep tr] is the list of patterns
 * contained in [tr].
 * @raise Failure if [tr] does not correspond to one of the following
 * structures:
 * - [tr] is a valid pattern parse tree. {b See:} [convert_pattern]
 * - [tr] is a [Node] with the following elements
 * {ol
 * {li a valid semicolon separated pattern parse tree}
 * {li a [SemiColon] token}
 * {li the pattern}
 * }
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
 * [convert_one_or_more_patterns_comma_sep tr] is the list of patterns
 * contained in [tr].
 * @raise Failure if [tr] does not correspond to one of the following
 * structures:
 * - [tr] is a valid pattern parse tree. {b See:} [convert_pattern]
 * - [tr] is a [Node] with the following elements
 * {ol
 * {li a valid comma separated pattern parse tree}
 * {li a [Comma] token}
 * {li the pattern}
 * }
 *)
and convert_one_or_more_patterns_comma_sep = function
  | Node [
      patterns_comma_sep;
      Token (Comma);
      pat;
    ] ->
      convert_pattern pat::
      convert_one_or_more_patterns_comma_sep patterns_comma_sep
  | pat -> [convert_pattern pat]

(**
 * [get_patterns tr acc] is the list of patterns in [tr] appended to [acc].
 *
 * @raise Failure if [tr] does not correspond to one of the following
 * structures:
 * - [tr] is a valid pattern. {b See:} [convert_pattern]
 * - [tr] is a Node where the first element is a valid parse tree according to
 * this definition and the second node is a valid pattern
 * ({b See:} [convert_pattern]).
 *)
let rec get_patterns one_or_more_patterns acc =
  try convert_pattern one_or_more_patterns::acc
  with _ ->
    match one_or_more_patterns with
    | Node [params_ptree; pat] ->
      get_patterns params_ptree ((convert_pattern pat)::acc)
    | _ -> failwith "not a valid one or more patterns"

(**
 * [convert_let_binding is_rec t] is the AST representing the let binding
 * represented by parse_tree [t].
 *
 * [is_rec] is true if the let binding is recursive and false otherwise.
 *
 * @raise Failure if [t] does not correspond to one of the following
 * structures:
 * - [t] is a function decl pattern parse tree with the elements of [Node]:
 * {ol
 * {li a [LowercaseIdent] token that describes the function name}
 * {li the pattern arguments parse tree. {b See:} get_patterns }
 * {li a [Equal] token}
 * {li an expr parse tree that contains the body expression of the function.
 * {b See:} convert_expr}
 * }
 * - [t] is a normal let decl pattern parse tree with the elements of [Node]:
 * {ol
 * {li the pattern that binds the variable. {b See:} get_patterns }
 * {li a [Equal] token}
 * {li an expr parse tree that contains the value expression of the declaration.
 * {b See:} convert_expr}
 * }
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
        convert_expr let_expr,
        true
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

(** [convert_list_body_expr acc t] is the list of AST expression nodes
 * in the parse tree [t] preprended to acc, where all expressions are
 * seperated by a semicolon.
 *
 * @raise Failure if [t] does not correspond to one of the following structures:
 * - [t] is a parse tree with the elements of [Node]:
 * {ol
 * {li a list body expression that follows the structure of this definition.}
 * {li a [SemiColon] token. }
 * {li an expr parse tree that contains the last element of the list.
 * {b See:} convert_expr}
 * }
 * - [t] is a valid expression. (b See:) [convert_expr]
 *
 * {b Example}: {v convert_list_body_expr
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
 *  v}
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

(** [convert_tuple_body_expr acc t] is the list of AST expression nodes
 * in the parse tree [t] preprended to acc, where all expressions are
 * seperated by a comma.
 *
 * @raise Failure if [t] does not correspond to one of the following structures:
 * - [t] is a parse tree with the elements of [Node]:
 * {ol
 * {li a tuple body expression that follows the structure of this definition.}
 * {li a [Comma] token. }
 * {li an expr parse tree that contains the last element of the tuple.
 * {b See:} convert_expr}
 * }
 * - [t] is a valid expression. (b See:) [convert_expr]
 *
 * {b Example}: {v convert_list_body_expr
 *   []
 *    ( Node [
 *       Node [
 *         Token (Int 1);
 *         Token (Comma);
 *         Token (Int 2);
 *       ];
 *       Token (Comma);
 *       Token (Int 3);
 *     ];)

 *  v}
 * gives the AST list:
 * [Expr Constant (Int 1), Expr Constant (Int 2), Expr Constant (Int 3)].
 *)
and convert_tuple_body_expr acc = function
  | Node [
      one_or_more_expr;
      Token (Token.Comma);
      expr;
    ] -> convert_tuple_body_expr ((convert_expr expr)::acc) one_or_more_expr
  | expr -> (convert_expr expr)::acc

(**
 * [convert_pattern_matching acc t] is the AST list of tuples
 * [(pattern, value, Some guard)] representing the match cases in [t]
 * prepended to [acc].
 *
 * @raise Failure if [t] does not match one of the following structures:
 * - [t] is a [Node] with the elements being:
 * {ol
 * {li An optional [VerticalBar] token. }
 * {li The pattern parse tree being matched against. {b See:}
 * [convert_pattern].}
 * {li a [FunctionArrow] token. }
 * {li the result expr parse tree that contains the expression to return if the
 * pattern matches. {b See:} convert_expr}
 * {li An optional further pattern matching case that follows the structure
 * defined here.}
 * }
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


and convert_field_binding = function
  | Node
      [Token (LowercaseIdent field_name);
       Token (Equal);
       value;
      ] -> (field_name, convert_expr value)
  | _ -> failwith "convert_field_binding called on a non-field-binding"

and convert_one_or_more_semicolon_separated_field_binding acc = function
  | Node [
      semicolon_sep_binding;
      Token (SemiColon);
      field_binding
    ] ->
    convert_one_or_more_semicolon_separated_field_binding
      ((convert_field_binding field_binding):: acc )
      semicolon_sep_binding
  | single_field_binding ->
    ((convert_field_binding single_field_binding) :: acc)

and get_record_field_binding_pairs record_type_node =
  match record_type_node with
  | Node children ->
    begin match children with
      | [Token (LCurlyBrace);
         semicolon_sep_fields;
         Token (RCurlyBrace)]
      | [Token (LCurlyBrace);
         semicolon_sep_fields;
         Token (RCurlyBrace);
         Token (SemiColon)] ->
        (convert_one_or_more_semicolon_separated_field_binding
           []
           semicolon_sep_fields)
      | _ -> failwith ("get_record_field_binding_pairs called on node that "^
                       "is not a record field body")
    end
  | _ -> failwith "get_record_field_binding_pairs called on leaf"


(**
 * [convert_expr tr] is the abstract syntax tree representing the expression in
 * [tr].
 *
 * @raise Failure if [t] does not conform to one of the following structures:
 * - An [Int], [Bool], [Float], [CharLiteral], [StringLiteral], [EmptyList],
 * [LowercaseIdent], [Unit] or token.
 * - A module accessor parse tree with the following elements:
 * {ol
 * {li The [CapitalizedIdent] describing the module name. }
 * {li A [Period] token. }
 * {li The [LowercaseIdent] describing the value accessed from that module.}
 * }
 * - A parenthesized expression parse tree with the following elements:
 * {ol
 * {li An [LParen] token. }
 * {li A valid expression parse tree according to these definitions. }
 * {li An [RParen] token. }
 * }
 * - A prefix expression parse tree with the following elements:
 * {ol
 * {li A token that was tagged as "prefix" }
 * {li A valid expression parse tree according to these definitions. }
 * }
 * - An infix expression parse tree with the following elements:
 * {ol
 * {li A valid expression parse tree according to these definitions. }
 * {li A token that was tagged as "infix" }
 * {li A valid expression parse tree according to these definitions. }
 * }
 * - An if expression parse tree with the following elements:
 * {ol
 * {li An [If] token. }
 * {li A valid expression parse tree according to these definitions that
 * represents the condition of the if expression. }
 * {li A [Then] token. }
 * {li A valid expression parse tree according to these definitions that
 * represents the true body expression. }
 * {li A optional [Else] token. }
 * {li An optional expression parse tree according to these definitions that
 * represents the else body expression that should be provided. }
 * }
 * - An anonymous function expression parse tree with the following elements:
 * {ol
 * {li An [Fun] token. }
 * {li A pattern list parse tree. {b See:} [get_patterns] }
 * {li A [FunctionArrow] token. }
 * {li A valid expression parse tree according to these definitions that
 * represents the body. }
 * }
 * - A semicolon expression parse tree with the following elements:
 * {ol
 * {li A valid expression parse tree according to these definitions that
 * represents the first expression. }
 * {li A [SemiColon] token. }
 * {li A valid expression parse tree according to these definitions that
 * represents the second expression and return value. }
 * }
 * - A let expression parse tree with the following elements:
 * {ol
 * {li A [Let] token. }
 * {li An optional [Rec] token. }
 * {li A let binding parse tree {b See:} [convert_let_binding]. }
 * {li A [In] keyword.}
 * {li A valid expression parse tree according to these definitions that
 * represents the expression where the binding occurs in. }
 * }
 * - A function call expression parse tree with the following elements:
 * {ol
 * {li A valid expression parse tree according to these definitions that
 * represents the function. }
 * {li A valid expression parse tree according to these definitions that
 * represents the argument to that function. }
 * }
 * - A list literal expression parse tree with the following elements:
 * {ol
 * {li A [StartList] token. }
 * {li A valid list body parse tree. {b See:} [convert_list_body_expr]. }
 * {li An optional [SemiColon] token. }
 * {li An [EndList] token. }
 * }
 * - A match expression parse tree with the following elements:
 * {ol
 * {li A [Match] token. }
 * {li A valid expression parse tree according to these definitions that
 * represents the value being matched against. }
 * {li An [With] token. }
 * {li A pattern matching cases parse tree is {b See:}
 * [convert_pattern_matching] }
 * }
 *)
and convert_expr tr = match tr with
  | Token (Token.Int v) -> Constant (Int v)
  | Token (Token.Bool b) -> Constant (Bool b)
  | Token (Token.Float v) -> Constant (Float v)
  | Token (Token.CharLiteral v) -> Constant (CharLiteral v)
  | Token (Token.StringLiteral v) -> Constant (StringLiteral v)
  | Token (Token.EmptyList) -> Constant (EmptyList)
  | Token (Token.EmptyArray) -> Constant (EmptyArray)
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
      _;
      Token (Token.Comma);
      _;
    ] as tr ->
      Tuple (convert_tuple_body_expr [] tr)

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
    ] ->
      LetBinding (
        convert_let_binding true let_binding,
        convert_expr let_expr
      )
  | Node [
      Token(Token.Let);
      let_binding;
      Token (Token.In);
      let_expr;
    ] ->
      LetBinding (
        convert_let_binding false let_binding,
        convert_expr let_expr
      )
  | Token (CapitalizedIdent constr) -> Variant (constr, None)
  | Node [
      Token (CapitalizedIdent constr);
      var_expr
    ] -> Variant (constr, Some (convert_expr var_expr))
  | Node [
      fun_expr;
      arg_expr;
    ] ->
      FunctionCall (convert_expr fun_expr, [convert_expr arg_expr], true)
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
      Token (Token.LCurlyBrace);
      one_or_more_field_bindings;
      Token (Token.RCurlyBrace);
    ]
  | Node [
      Token (Token.LCurlyBrace);
      one_or_more_field_bindings;
      Token (Token.SemiColon);
      Token (Token.RCurlyBrace);
    ] -> Record
           (convert_one_or_more_semicolon_separated_field_binding
              []
              one_or_more_field_bindings
           )
  | Node [
        Token (Token.StartArray);
        one_or_more_expr;
        Token (Token.EndArray);
      ]
    | Node [
        Token (Token.StartArray);
        one_or_more_expr;
        Token (Token.SemiColon);
        Token (Token.EndArray);
      ] ->
        ArrayExpr (convert_list_body_expr [] one_or_more_expr)
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

let rec convert_typexpr = function (* list of tuple items *)
| Node [
    Token (LParen);
    typexpr;
    Token (RParen);
  ] -> convert_typexpr typexpr
| Node [
    typexpr;
    Token (Times);
    more_typexprs
  ] ->
    let tl = match convert_typexpr more_typexprs with
    | Type _ as tr -> [tr]
    | TypeTuple lst -> lst
    in
    TypeTuple ((convert_typexpr typexpr)::tl)
| Token (LowercaseIdent constr) -> Type constr
| _ -> failwith "not valid typexpr"

let convert_constr_decl = function
| Token (CapitalizedIdent constr) -> (constr, None)
| Node [
    Token (CapitalizedIdent constr);
    Token (Of);
    typexpr
  ] -> (constr, Some (convert_typexpr typexpr))
| _ -> failwith "not a valid constr decl"

let rec convert_constr_decl_vert_bar_sep = function
| Node [
    Token (VerticalBar);
    constr_tr;
    further_constr_decls;
  ] ->
    (convert_constr_decl constr_tr)::
    (convert_constr_decl_vert_bar_sep further_constr_decls)
| Node [
    Token (VerticalBar);
    constr_tr;
  ] -> [convert_constr_decl constr_tr]
| _ -> failwith "not valid constr decls with vertical bar sep"

let rec convert_repr tr =
  try [convert_constr_decl tr] with _ ->
  try convert_constr_decl_vert_bar_sep tr with _ ->
  match tr with
  | Node [
      constr_decl_tr;
      constr_decl_vert_bar_tr;
    ] ->
      let lst = convert_constr_decl_vert_bar_sep constr_decl_vert_bar_tr in
      let constr = convert_constr_decl constr_decl_tr in
      constr::lst
  | _ -> failwith "not a valid representation"



let convert_field_decl = function
  | Node
      [Token (LowercaseIdent field_name);
       Token (Colon);
       field_type;
      ] -> (field_name, convert_typexpr field_type)
  | _ -> failwith "convert_field_decl called on a non-field-decl"

let rec convert_one_or_more_semicolon_separated_field_decl acc = function
  | Node [
      semicolon_sep_decl;
      Token (SemiColon);
      field_binding
    ] ->
    convert_one_or_more_semicolon_separated_field_decl
      ((convert_field_decl field_binding):: acc )
      semicolon_sep_decl
  | single_field_binding ->
    ((convert_field_decl single_field_binding) :: acc)

let get_record_field_type_pairs record_type_node =
  match record_type_node with
  | Node children ->
    begin match children with
    | [Token (LCurlyBrace);
       semicolon_sep_fields;
       Token (RCurlyBrace)]
    | [Token (LCurlyBrace);
       semicolon_sep_fields;
       Token (RCurlyBrace);
       Token (SemiColon)] ->
      (convert_one_or_more_semicolon_separated_field_decl
         []
         semicolon_sep_fields)
    | _ -> failwith ("get_record_field_type_pairs called on node that "^
                     "is not a record type declaration")
    end
  | _ -> failwith "get_record_field_pairs called on leaf"

(**
 * [convert_definition t] is the abstract syntax tree representing the
 * a definition (an [open Module] or [let var_name = expr]) in the top level
 * of a module.
 *
 * @raise Failure if [t] does not conform to one of the following structures:
 * - An open declaration [Node] with the following elements:
 * {ol
 * {li An [Open] token. }
 * {li A [CapitalizedIdent] token representing the module name. }
 * }
 * - A let binding with the following elements
 * {ol
 * {li An [Let] token. }
 * {li An optional [Rec] token.}
 * {li A valid let binding. {b See:} [convert_let_binding] }
 * }
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
      Token (Type);
      Token (LowercaseIdent typ);
      Token (Equal);
      repr
    ] ->
    (try
       TypeDefinition (typ, VariantDecl (convert_repr repr))
     with _ ->
       TypeDefinition
         (typ,
          RecordDecl
            (repr |>
             get_record_field_type_pairs
            )
         )
    )
  | Node [
      Token(Token.Let);
      let_binding;
    ] ->
      LetDecl (convert_let_binding false let_binding)
  | _ -> failwith "not a valid definition"


(**
 * [convert_module_item tr] is the expression or declaration in [tr].
 *
 * @raise Failure if [t] does not conform to one of the following structures:
 * - [tr] is a valid expression. {b See:} [convert_expr]
 * - [tr] is a valid definition. {b See:} [convert_definition]
 *)
let convert_module_item tr =
  try Expr (convert_expr tr) with _ -> (convert_definition tr)

(**
 * [convert_one_plus_def_expr tr] is the list of declarations and expressions
 * of [tr] where [tr] represents one or more definitions or expressions.
 *
 * @raise Failure if [t] does not conform to one of the following structures:
 * - A module item parse tree with the following structure:
 * {ol
 * {li An optional [DoubleSemicolon] token. }
 * {li A valid module item tree. {b See:} [convert_module_item].}
 * {li An optional further parse tree that conforms to these definitions. }
 * }
 * - A single module item tree. {b See:} [convert_module_item]
 *)
let rec convert_one_plus_def_expr = function
  | Token (DoubleSemicolon) -> []
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
 * declarations and expressions of [tr].
 *
 * @raise Failure if [t] does not conform to one of the following structures:
 * - A module item parse tree with the following structure:
 * {ol
 * {li A valid module item tree. {b See:} [convert_module_item].}
 * {li An optional further parse tree that conforms to these definitions. }
 * {li An optional [DoubleSemicolon] token. }
 * }
 * - A single module item tree. {b See:} [convert_module_item]
 *)
let convert_expr_definition_no_start_double_semicolon tr =
    try [convert_module_item tr] with _ ->
      match tr with
        | Node [
            tr;
            def_expr_tr;
          ] -> (convert_module_item tr)::(convert_one_plus_def_expr def_expr_tr)
        | _ -> failwith "not a valid expr def no start ;;"

(**
 * [convert_ast tr] is the list of declarations and expressions of the module
 * parse tree [tr].
 *
 * @raise Failure if [tr] is not a valid module expression or declaration parse
 * tree.
 *)
let convert_ast = function
  | Node [
      Token (DoubleSemicolon);
      tr;
    ]
  | tr -> convert_expr_definition_no_start_double_semicolon tr

let convert parse_tr =
  convert_ast parse_tr
