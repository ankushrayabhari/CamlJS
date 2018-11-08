open Ast

let rec optimize_let_binding = function
  | VarAssignment (pat, expr) -> VarAssignment (pat, optimize_expr expr)
  | FunctionAssignment (name, is_rec, args, body, curry) ->
      FunctionAssignment (name, is_rec, args, optimize_expr body, curry)
  | TailRecursiveFunctionAssignment (name, args, body) ->
      TailRecursiveFunctionAssignment (name, args, optimize_expr body)

and optimize_expr = function
  | FunctionCall (fun_expr, args, curry) ->
      let optimized_fun = optimize_expr fun_expr in
      let optimized_args = List.map optimize_expr args in
      begin match optimized_fun with
        | FunctionCall (f, first_args, c) ->
            FunctionCall (f, first_args@optimized_args, curry)
        | t -> FunctionCall (t, optimized_args, curry)
      end
  | (Constant _ as t)
  | (VarName _ as t)
  | (ModuleAccessor _ as t)
  | (Variant (_, None) as t) -> t
  | PrefixOp (pref, expr) -> PrefixOp (pref, optimize_expr expr)
  | InfixOp (l, op, r) -> InfixOp (optimize_expr l, op, optimize_expr r)
  | Ternary (cond, then_expr, Some else_expr) ->
      Ternary (optimize_expr cond, optimize_expr then_expr,
               Some (optimize_expr else_expr))
  | Ternary (cond, then_expr, None) ->
      Ternary (optimize_expr cond, optimize_expr then_expr, None)
  | Function (args, body) ->
      Function (args, optimize_expr body)
  | Sequential (l, r) -> Sequential (optimize_expr l, optimize_expr r)
  | LetBinding (l, in_expr) -> LetBinding (optimize_let_binding l, optimize_expr in_expr)
  | ParenExpr expr -> ParenExpr (optimize_expr expr)
  | ((ListExpr lst) as tr)
  | ((ArrayExpr lst) as tr)
  | ((Tuple lst) as tr) -> begin
      let optimized_lst = List.map optimize_expr lst in
      match tr with
      | ListExpr _ -> ListExpr optimized_lst
      | ArrayExpr _ -> ArrayExpr optimized_lst
      | Tuple _ -> Tuple optimized_lst
      | _ -> failwith "should not be called"
  end
  | MatchExpr (target, match_cases_lst) ->
      MatchExpr (
        optimize_expr target,
        List.map (fun el -> match el with
          | (pat, value, Some guard) ->
              (pat, optimize_expr value, Some (optimize_expr guard))
          | (pat, value, None) -> (pat, optimize_expr value, None)
        ) match_cases_lst
      )
  | Record lst ->
      Record (List.map (fun (name, value) -> (name, optimize_expr value)) lst)
  | Variant (constr, Some arg) -> Variant (constr, Some (optimize_expr arg))
  | PropertyAccessor (expr, prop) -> PropertyAccessor (optimize_expr expr, prop)

let optimize tr =
  List.map (fun module_item ->
    match module_item with
    | Expr e -> Expr (optimize_expr e)
    | LetDecl let_binding -> LetDecl (optimize_let_binding let_binding)
    | t -> t
  ) tr
