open Ast

let functions = Hashtbl.create 1000;;

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
      check_no_calls_or_references name body
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
  | LetBinding (VarAssignment (_, _, assign_expr), in_expr) ->
      check_no_calls_or_references name assign_expr &&
      check_no_calls_or_references name in_expr
  | LetBinding (FunctionAssignment (_, _, _, body, _), in_expr)
  | LetBinding (TailRecursiveFunctionAssignment (_, _, body), in_expr) ->
      check_no_calls_or_references name body &&
      check_no_calls_or_references name in_expr
  | MatchExpr (target_expr, pat_lst) ->
      check_no_calls_or_references name target_expr &&
      List.fold_left (fun acc (pat, value, guard_opt) ->
        let valid_guard = match guard_opt with
        | Some guard -> check_no_calls_or_references name guard
        | None -> true
        in
        acc && valid_guard && check_no_calls_or_references name value
      ) true pat_lst
  | PropertyAccessor (expr, _) -> check_no_calls_or_references name expr

let rec check_function_body name = function
  | Ternary (cond, then_body_expr, Some else_body_expr) ->
      check_no_calls_or_references name cond &&
      check_function_body name then_body_expr &&
      check_function_body name else_body_expr
  | MatchExpr (target_expr, pat_lst) ->
      check_no_calls_or_references name target_expr &&
      List.fold_left (fun acc (pat, value, guard_opt) ->
        let valid_guard = match guard_opt with
        | Some guard -> check_no_calls_or_references name guard
        | None -> true
        in
        acc && valid_guard && check_function_body name value
      ) true pat_lst
  | FunctionCall (VarName name, arg_lst, false) ->
      List.fold_left (fun acc arg ->
        acc && check_function_body name arg
      ) true arg_lst
  | ParenExpr p -> check_function_body name p
  | Sequential (l, r) ->
      check_no_calls_or_references name l &&
      check_function_body name r
  | t -> check_no_calls_or_references name t

let rec detect_let_decl = function
  | VarAssignment (pat, is_rec, assign_expr) ->
      detect_expr assign_expr
  | FunctionAssignment (name, true, pat_lst, body, false) ->
      if check_function_body name body then Hashtbl.add functions name ()
  | FunctionAssignment (name, _, pat_lst, body, _)
  | TailRecursiveFunctionAssignment (name, pat_lst, body) ->
      detect_expr body

and detect_expr = function
  | LetBinding (let_bind, in_expr) ->
      detect_let_decl let_bind;
      detect_expr in_expr
  | FunctionCall (fun_expr, arg_lst, curried) ->
      List.iter detect_expr arg_lst;
      detect_expr fun_expr
  | Function (pat_lst, body_expr) ->
      detect_expr body_expr
  | MatchExpr (target, match_cases) ->
      detect_expr target;
      List.iter (fun el ->
        match el with
          | (pat, value, Some guard) ->
              detect_expr value;
              detect_expr guard
          | (pat, value, None) ->
              detect_expr value
      ) match_cases
  | PrefixOp (_, expr) -> detect_expr expr
  | InfixOp (l_expr, _, r_expr) ->
      detect_expr l_expr;
      detect_expr r_expr
  | Ternary (cond_expr, then_expr, Some else_expr) ->
      detect_expr cond_expr;
      detect_expr then_expr;
      detect_expr else_expr
  | Ternary (cond_expr, then_expr, None) ->
      detect_expr cond_expr;
      detect_expr then_expr
  | Sequential (expr1, expr2) ->
      detect_expr expr1;
      detect_expr expr2
  | ParenExpr expr -> detect_expr expr
  | ListExpr lst
  | ArrayExpr lst
  | Tuple lst -> List.iter detect_expr lst
  | Record lst ->
      List.iter (fun (_, value) -> detect_expr value) lst
  | Variant (_, Some arg) -> detect_expr arg
  | PropertyAccessor (expr, _) -> detect_expr expr
  | VarName _
  | Constant _
  | ModuleAccessor (_, _)
  | Variant (_, None) -> ()

let rec optimize_let_decl = function
  | VarAssignment (pat, is_rec, assign_expr) ->
      VarAssignment (pat, is_rec, optimize_expr assign_expr)
  | FunctionAssignment (name, _, pat_lst, body, _)
    when Hashtbl.mem functions name ->
      TailRecursiveFunctionAssignment (name, pat_lst, optimize_expr body)
  | FunctionAssignment (name, is_rec, pat_lst, body, curry) ->
      FunctionAssignment (name, is_rec, pat_lst, optimize_expr body, curry)
  | TailRecursiveFunctionAssignment (name, pat_lst, body) ->
      TailRecursiveFunctionAssignment (name, pat_lst, optimize_expr body)

and optimize_expr = function
  | FunctionCall (fun_expr, arg_lst, curried) ->
      FunctionCall (optimize_expr fun_expr, List.map optimize_expr arg_lst, curried)
  | LetBinding (let_decl, in_expr) ->
      LetBinding (
        optimize_let_decl let_decl,
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
  | PropertyAccessor (expr, prop) -> PropertyAccessor (optimize_expr expr, prop)

let optimize tr =
  List.iter (fun el -> match el with
    | Expr e -> detect_expr e
    | LetDecl l -> detect_let_decl l
    | t -> ()
  ) tr;
  List.map (fun el -> match el with
    | Expr e -> Expr (optimize_expr e)
    | LetDecl l -> LetDecl (optimize_let_decl l)
    | t -> t
  ) tr
