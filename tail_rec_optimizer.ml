open Ast

let rec name_bound_in_pat name = function
  | IgnorePattern
  | RangedCharacterPattern (_, _)
  | ConstantPattern _
  | VariantPattern (_, None) -> true
  | AliasPattern (pat, alias) -> alias = name || name_bound_in_pat name pat
  | ValueNamePattern str -> str = name
  | ParenPattern pat -> name_bound_in_pat name pat
  | ListPattern pat_lst
  | TuplePattern pat_lst
  | ArrayPattern pat_lst ->
      List.fold_left (fun acc arg ->
        acc && name_bound_in_pat name arg
      ) true pat_lst
  | ConsPattern (pat1, pat2) ->
      name_bound_in_pat name pat1 && name_bound_in_pat name pat2
  | VariantPattern (_, Some pat) -> name_bound_in_pat name pat
  | RecordPattern param_lst ->
      List.fold_left (fun acc (_, pat) ->
        acc && name_bound_in_pat name pat
      ) true param_lst

let rec check_no_calls_or_references name = function
  | Constant _
  | ModuleAccessor (_, _)
  | Variant (_, None) -> true
  | VarName str -> str <> name
  | PrefixOp (_, expr) -> check_no_calls_or_references name expr
  | InfixOp (l, op, r) ->
      check_no_calls_or_references name l &&
      check_no_calls_or_references name r
  | Ternary (cond, then_body_expr, else_body_expr) ->
      check_no_calls_or_references name cond &&
      check_no_calls_or_references name then_body_expr &&
      begin match else_body_expr with
        | None -> true
        | Some else_body_expr -> check_no_calls_or_references name else_body_expr
      end
  | Function (pat_lst, body) ->
      let name_bound = List.fold_left (fun acc pat ->
          acc && name_bound_in_pat name pat
        ) true pat_lst
      in
      name_bound || check_no_calls_or_references name body
  | Sequential (l, r) ->
      check_no_calls_or_references name l &&
      check_no_calls_or_references name r
  | FunctionCall (func, arg_lst, _) ->
      check_no_calls_or_references name func &&
      List.fold_left (fun acc arg ->
        acc && check_no_calls_or_references name arg
      ) true arg_lst
  | ParenExpr expr -> check_no_calls_or_references name expr
  | ListExpr lst | ArrayExpr lst | Tuple lst ->
      List.fold_left (fun acc arg ->
        acc && check_no_calls_or_references name arg
      ) true lst
  | Record param_lst ->
      List.fold_left (fun acc (_, arg) ->
        acc && check_no_calls_or_references name arg
      ) true param_lst
  | Variant (_, Some expr) -> check_no_calls_or_references name expr
  | LetBinding (VarAssignment (pat, assign_expr), in_expr) ->
      check_no_calls_or_references name assign_expr &&
      (name_bound_in_pat name pat || check_no_calls_or_references name in_expr)
  | LetBinding (FunctionAssignment (fn, is_rec, args, body, _), in_expr) ->
      let name_bound_in_args =
        List.fold_left (fun acc pat ->
            acc && name_bound_in_pat name pat
        ) true args
      in
      let same_name = name = fn in
      let valid_in_expr =
        (same_name || check_no_calls_or_references name in_expr) in
      let valid_body =
        (is_rec && same_name || name_bound_in_args) ||
        check_no_calls_or_references name body
      in
      valid_in_expr && valid_body
  | LetBinding (TailRecursiveFunctionAssignment (fn, args, body), in_expr) ->
      let name_bound_in_args =
        List.fold_left (fun acc pat ->
            acc && name_bound_in_pat name pat
        ) true args
      in
      let same_name = name = fn in
      let valid_in_expr =
        (same_name || check_no_calls_or_references name in_expr) in
      let valid_body =
        (same_name || name_bound_in_args) ||
        check_no_calls_or_references name body
      in
      valid_in_expr && valid_body
  | MatchExpr (target_expr, pat_lst) ->
      check_no_calls_or_references name target_expr &&
      List.fold_left (fun acc (pat, value, guard_opt) ->
        let valid_guard = match guard_opt with
        | Some guard -> check_no_calls_or_references name guard
        | None -> true
        in
        acc && valid_guard &&
        (name_bound_in_pat name pat || check_no_calls_or_references name value)
      ) true pat_lst

let rec valid_tail_rec_func name = function
  | Ternary (cond, then_body_expr, Some else_body_expr) ->
      check_no_calls_or_references name cond &&
      valid_tail_rec_func name then_body_expr &&
      valid_tail_rec_func name else_body_expr
  | MatchExpr (target_expr, pat_lst) ->
      check_no_calls_or_references name target_expr &&
      List.fold_left (fun acc (pat, value, guard_opt) ->
        let valid_guard = match guard_opt with
        | Some guard -> check_no_calls_or_references name guard
        | None -> true
        in
        acc && valid_guard && valid_tail_rec_func name value
      ) true pat_lst
  | FunctionCall (VarName name, arg_lst, false) ->
      List.fold_left (fun acc arg ->
        acc && check_no_calls_or_references name arg
      ) true arg_lst
  | t -> false

let rec optimize_expr = function
  | LetBinding (FunctionAssignment (name, true, args, body, false), in_expr) ->
      if valid_tail_rec_func name body then
        LetBinding (
          TailRecursiveFunctionAssignment (name, args, body),
          optimize_expr in_expr
        )
      else
        LetBinding (
          FunctionAssignment (name, true, args, body, false),
          optimize_expr in_expr
        )
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
    | LetDecl (FunctionAssignment (name, true, args, body, false)) as tr ->
        if valid_tail_rec_func name body then
          LetDecl (TailRecursiveFunctionAssignment (name, args, body))
        else
          tr
    | t -> t
  ) tr
