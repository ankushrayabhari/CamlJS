open Parser

type constant =
  | Int of int

type infix_op =
  | Plus | Minus | Divide | Times | GreaterThan | LessThan | GreaterThanOrEqual
  | LessThanOrEqual | Equal | NotEqual

type prefix_symbol =
  | Negation

type value_name =
  | LowercaseIdent of string

type pattern =
  | ValueName of value_name

type let_binding =
  | VarAssignment of pattern * expr
  | FunctionAssignment of value_name * bool * pattern list * expr
and expr =
  | Constant of constant
  | PrefixOp of prefix_symbol * expr
  | InfixOp of expr * infix_op * expr
  | Ternary of expr * expr * expr option
  | Function of pattern list * expr
  | Sequential of expr * expr
  | LetBinding of let_binding * expr
  | VarName of value_name
  | FunctionCall of expr * expr
  | ParenExpr of expr
type ast = expr

let convert_prefix = function
  | Tokenizer.Negation -> Negation
  | t -> failwith (
      "prefix operator conversion not supported: " ^
      (Tokenizer.token_to_string t)
    )

let convert_infix = function
  | Tokenizer.Plus -> Plus
  | Tokenizer.Minus -> Minus
  | Tokenizer.Times -> Times
  | Tokenizer.Divide -> Divide
  | Tokenizer.GreaterThan -> GreaterThan
  | Tokenizer.LessThan -> LessThan
  | Tokenizer.GreaterThanOrEqual -> GreaterThanOrEqual
  | Tokenizer.LessThanOrEqual -> LessThanOrEqual
  | Tokenizer.NotEqual -> NotEqual
  | Tokenizer.Equal -> Equal
  | t -> failwith (
      "infix operator conversion not supported: " ^
      (Tokenizer.token_to_string t)
    )

let unbox_param acc param =
  match param with
  | Token (Tokenizer.LowercaseIdent p) -> (ValueName (LowercaseIdent p))::acc
  | _ -> failwith "not a valid parameter"

let rec convert_expr t = match t with
  | Token (Tokenizer.Int v) -> Constant (Int v)

  | Node [
      Token(Tokenizer.LParen);
      expr;
      Token(Tokenizer.RParen);
    ] ->
    ParenExpr (convert_expr expr)

  | Node [
      Token(pre);
      expr;
    ] when Tokenizer.has_tag pre "prefix" ->
    PrefixOp (convert_prefix pre, convert_expr expr)

  | Node [
      expr1;
      Token(infix);
      expr2;
    ] when Tokenizer.has_tag infix "infix" ->
    InfixOp (convert_expr expr1, convert_infix infix, convert_expr expr2)

  | Node [
      Token(Tokenizer.If);
      cond_expr;
      Token(Tokenizer.Then);
      then_expr;
    ] ->
    Ternary (convert_expr cond_expr, convert_expr then_expr, None)

  | Node [
      Token(Tokenizer.If);
      cond_expr;
      Token(Tokenizer.Then);
      then_expr;
      Token(Tokenizer.Else);
      else_expr;
    ] ->
    Ternary (
      convert_expr cond_expr,
      convert_expr then_expr,
      Some (convert_expr else_expr)
    )

  | Node [
      Token(Tokenizer.Fun);
      one_or_more_params;
      Token(Tokenizer.FunctionArrow);
      anon_func_expr
    ] ->
    let params =
      match one_or_more_params with
      | Token (Tokenizer.LowercaseIdent p) -> [ValueName (LowercaseIdent p)]
      | Node params_ptree -> List.rev (List.fold_left unbox_param [] params_ptree)
      | _ -> failwith "not a valid oneOrMoreParams"
    in Function (params, convert_expr anon_func_expr)

  | Node [
      expr1;
      Token(Tokenizer.SemiColon);
      expr2;
    ] -> Sequential (convert_expr expr1, convert_expr expr2)

  | Node (Token(Tokenizer.Let)::t) ->
    failwith "let binding not implemented"

  | Token (Tokenizer.LowercaseIdent n) ->
    VarName (LowercaseIdent n)

  | Node [
      fun_expr;
      arg_expr;
    ] ->
    FunctionCall (convert_expr fun_expr, convert_expr arg_expr)

  | _ -> failwith "not a valid expression"

let convert parse_tr =
  convert_expr parse_tr
