open Parser
open Ast

let convert_prefix = function
  | Token.Negation -> Negation
  | t -> failwith (
      "prefix operator conversion not supported: " ^
      (Tokenizer.token_to_string t)
    )

let convert_infix = function
  | Token.Plus -> Plus
  | Token.Minus -> Minus
  | Token.Times -> Times
  | Token.Divide -> Divide
  | Token.GreaterThan -> GreaterThan
  | Token.LessThan -> LessThan
  | Token.GreaterThanOrEqual -> GreaterThanOrEqual
  | Token.LessThanOrEqual -> LessThanOrEqual
  | Token.NotEqual -> NotEqual
  | Token.Equal -> Equal
  | Token.Append -> Append
  | Token.Cons -> Cons
  | t -> failwith (
      "infix operator conversion not supported: " ^
      (Tokenizer.token_to_string t)
    )

let convert_pattern = function
  | Token (Token.LowercaseIdent ident_name) ->
    ValueName (ident_name)
  | _ -> failwith "not a valid pattern"

let rec get_patterns one_or_more_patterns acc =
  match one_or_more_patterns with
  | Node [params_ptree; pat] ->
    get_patterns params_ptree ((convert_pattern pat)::acc)
  | Token _ as single -> convert_pattern single::acc
  | _ -> failwith "not a valid oneOrMorePatterns"

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

and convert_expr = function
  | Token (Token.Int v) -> Constant (Int v)

  | Token (Token.EmptyList) -> Constant (EmptyList)

  | Token (Token.LowercaseIdent n) -> VarName n

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

  | _ -> failwith "not a valid expression"

let convert parse_tr =
  convert_expr parse_tr
