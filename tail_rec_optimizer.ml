open Ast

let optimize_function_assign = function
  | FunctionAssignment (name, true, args, body_expr, true) as tr -> tr
  | tr -> tr

let rec optimize_expr = function
  | LetBinding (FunctionAssignment (_, true, _, _, true) as func, in_expr) ->
      LetBinding (optimize_function_assign func, optimize_expr in_expr)
  | (Constant _ as tr)
  | (VarName _ as tr)
  | (ModuleAccessor (_, _) as tr)
  | (Variant (_, None) as tr) -> tr
  | PrefixOp (pref, expr) -> PrefixOp (pref, optimize_expr expr)
  | InfixOp (l_expr, op, r_expr) ->
      InfixOp (optimize_expr l_expr, op, optimize_expr r_expr)
  | Ternary (cond_expr, then_expr, Some else_expr) ->
      Ternary (optimize_expr cond_expr, optimize_expr then_expr,
        Some (optimize_expr else_expr))
  | Ternary (cond_expr, then_expr, None) ->
      Ternary (optimize_expr cond_expr, optimize_expr then_expr, None)
  | Function (pat_lst, body_expr) ->
      Function (pat_lst, optimize_expr body_expr)
  | Sequential (expr1, expr2) ->
      Sequential (optimize_expr expr1, optimize_expr expr2)
  | LetBinding (VarAssignment (pat, assign_expr), in_expr) ->
      LetBinding (
        VarAssignment (pat, optimize_expr assign_expr),
        optimize_expr in_expr
      )
  | LetBinding (FunctionAssignment (name, is_rec, arg, body, curry), in_expr) ->
      LetBinding (
        FunctionAssignment (name, is_rec, arg, optimize_expr body, curry),
        optimize_expr in_expr
      )
  | LetBinding ((TailRecursiveFunctionAssignment _) as tr, in_expr) ->
      LetBinding (tr, optimize_expr in_expr)
  | FunctionCall (fun_expr, arg_lst, curried) ->
      FunctionCall (
        optimize_expr fun_expr,
        List.map optimize_expr arg_lst,
        curried
      )
  | ParenExpr expr -> ParenExpr (optimize_expr expr)
  | ListExpr lst -> ListExpr (List.map optimize_expr lst)
  | ArrayExpr lst -> ArrayExpr (List.map optimize_expr lst)
  | Tuple lst -> Tuple (List.map optimize_expr lst)
  | MatchExpr (target_expr, pat_lst) ->
      MatchExpr (
        optimize_expr target_expr,
        List.map (fun el -> match el with
          | (pat, value, Some guard) ->
              (pat, optimize_expr value, Some (optimize_expr guard))
          | (pat, value, None) -> (pat, optimize_expr value, None)
        ) pat_lst
      )
  | Record lst ->
      Record (List.map (fun (name, value) -> (name, optimize_expr value)) lst)
  | Variant (constr, Some arg) -> Variant (constr, Some (optimize_expr arg))


let optimize tr =
  List.map (fun module_item ->
    match module_item with
    | Expr expr ->
        Expr (optimize_expr expr)
    | LetDecl (FunctionAssignment (_, true, _, _, true) as func) ->
        LetDecl (optimize_function_assign func)
    | t -> t
  ) tr
