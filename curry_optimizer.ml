open Ast

let functions = Hashtbl.create 1000;;

let rec detect_let_decl = function
  | VarAssignment (pat, is_rec, assign_expr) ->
      detect_expr assign_expr
  | FunctionAssignment (name, _, pat_lst, body, _)
  | TailRecursiveFunctionAssignment (name, pat_lst, body) ->
      Hashtbl.add functions name (List.length pat_lst);
      detect_expr body

and detect_expr = function
  | LetBinding (let_bind, in_expr) ->
      detect_let_decl let_bind;
      detect_expr in_expr
  | FunctionCall (VarName str, arg_lst, curried) -> begin
      let is_mem = Hashtbl.mem functions str in
      if is_mem && List.length arg_lst >= Hashtbl.find functions str then begin
        List.iter detect_expr arg_lst;
      end else begin
        Hashtbl.remove functions str;
        List.iter detect_expr arg_lst
      end
  end
  | FunctionCall (fun_expr, arg_lst, curried) ->
      List.iter detect_expr arg_lst;
      detect_expr fun_expr
  | VarName str -> Hashtbl.remove functions str
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
  | Constant _
  | ModuleAccessor (_, _)
  | Variant (_, None) -> ()

let rec optimize_let_decl = function
  | VarAssignment (pat, is_rec, assign_expr) ->
      VarAssignment (pat, is_rec, optimize_expr assign_expr)
  | FunctionAssignment (name, is_rec, pat_lst, body, _) ->
      FunctionAssignment (name, is_rec, pat_lst, optimize_expr body,
        not (Hashtbl.mem functions name))
  | TailRecursiveFunctionAssignment (name, pat_lst, body) ->
      TailRecursiveFunctionAssignment (name, pat_lst, optimize_expr body)

and optimize_expr = function
  | FunctionCall (VarName str, arg_lst, curried) ->
      if Hashtbl.mem functions str then begin
        let args = Array.of_list (List.map optimize_expr arg_lst) in
        let uncurried_arg_length = Hashtbl.find functions str in
        let uncurried_args = Array.sub args 0 uncurried_arg_length |> Array.to_list in
        if Array.length args > uncurried_arg_length then begin
          let curried_args =
            Array.sub args uncurried_arg_length
            (Array.length args - uncurried_arg_length) |> Array.to_list in
          FunctionCall (FunctionCall (VarName str, uncurried_args, false), curried_args, true)
        end else begin
          FunctionCall (VarName str, uncurried_args, false)
        end
      end else begin
        FunctionCall (VarName str, List.map optimize_expr arg_lst, true)
      end
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
  List.map (fun el ->
    match el with
    | Expr e -> Expr (optimize_expr e)
    | LetDecl l -> LetDecl (optimize_let_decl l)
    | t -> t
  ) tr
