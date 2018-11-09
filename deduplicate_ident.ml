open Ast

let binding_stack = Hashtbl.create 10000;;

let curr_binding = ref "a";;

let incr_binding () =
  let last_idx = String.length !curr_binding - 1 in
  let curr_last_char = String.get !curr_binding last_idx in
  if curr_last_char = 'z' then
    curr_binding := String.make (String.length !curr_binding + 1) 'a'
  else
    let curr_last_char_code = Char.code curr_last_char in
    let next_last_char_code = curr_last_char_code + 1 in
    let next_last_char = Char.chr next_last_char_code in
    curr_binding := String.concat "" [
      String.sub !curr_binding 0 last_idx;
      String.make 1 next_last_char
    ]

let add_binding binding =
  let new_binding = !curr_binding in
  begin try
    let binding_lst = Hashtbl.find binding_stack binding in
    Hashtbl.add binding_stack binding (new_binding::binding_lst);
  with _ ->
    Hashtbl.add binding_stack binding [new_binding];
  end;
  incr_binding ();
  new_binding

let remove_binding binding =
  let binding_lst = Hashtbl.find binding_stack binding in
  begin match binding_lst with
    | _::[] -> Hashtbl.remove binding_stack binding
    | _::t -> begin
      Hashtbl.remove binding_stack binding;
      Hashtbl.add binding_stack binding t
    end
    | _ -> failwith "should not be empty"
  end;
  binding

let get_most_recent_binding ident =
  try
    Hashtbl.find binding_stack ident |> List.hd
  with _ ->
    ident

let rec pattern_binding_fold f = function
  | AliasPattern (pat, alias) ->
      AliasPattern (pattern_binding_fold f pat, f alias)
  | ValueNamePattern str ->
      ValueNamePattern (f str)
  | ParenPattern pat -> pattern_binding_fold f pat
  | ListPattern pat_lst ->
      ListPattern (List.map (pattern_binding_fold f) pat_lst)
  | TuplePattern pat_lst ->
      TuplePattern (List.map (pattern_binding_fold f) pat_lst)
  | ArrayPattern pat_lst ->
      ArrayPattern (List.map (pattern_binding_fold f) pat_lst)
  | ConsPattern (pat1, pat2) ->
      ConsPattern (pattern_binding_fold f pat1, pattern_binding_fold f pat2)
  | VariantPattern (constr, Some pat) ->
      VariantPattern (constr, Some (pattern_binding_fold f pat))
  | RecordPattern param_lst ->
      RecordPattern (List.map (fun (prop, pat) ->
        (prop, pattern_binding_fold f pat)
      ) param_lst)
  | (IgnorePattern as tr)
  | (RangedCharacterPattern (_, _) as tr)
  | (ConstantPattern _ as tr)
  | (VariantPattern (_, None) as tr) -> tr

let optimize_pattern = pattern_binding_fold add_binding;;
let remove_pattern_bindings = pattern_binding_fold remove_binding;;

let rec optimize_expr = function
  | VarName str ->
      VarName (get_most_recent_binding str)
  | Function (pat_lst, body_expr) ->
      let optimized_pat_lst = List.map optimize_pattern pat_lst in
      let optimized_body = optimize_expr body_expr in
      let optimized = Function (optimized_pat_lst, optimized_body) in
      List.map remove_pattern_bindings pat_lst |> ignore;
      optimized
  | LetBinding (VarAssignment (pat, is_rec, assign_expr), in_expr) ->
      let (optimized_pat, optimized_assign_expr, optimized_in_expr) =
        if is_rec then begin
          let optimized_pat = optimize_pattern pat in
          let optimized_assign = optimize_expr assign_expr in
          let optimized_in_expr = optimize_expr in_expr in
          remove_pattern_bindings pat |> ignore;
          (optimized_pat, optimized_assign, optimized_in_expr)
        end else begin
          let optimized_assign = optimize_expr assign_expr in
          let optimized_pat = optimize_pattern pat in
          let optimized_in_expr = optimize_expr in_expr in
          remove_pattern_bindings pat |> ignore;
          (optimized_pat, optimized_assign, optimized_in_expr)
        end
      in
      let optimized = LetBinding (
        VarAssignment (optimized_pat, is_rec, optimized_assign_expr),
        optimized_in_expr
      ) in
      remove_pattern_bindings pat |> ignore;
      optimized
  | LetBinding (
      FunctionAssignment (name, is_rec, pat_lst, body, curry), in_expr) ->
      let (optimized_name, optimized_pat_lst, optimized_body) =
        if is_rec then begin
          let optimized_name = add_binding name in
          let optimized_pat_lst = List.map optimize_pattern pat_lst in
          let optimized_body = optimize_expr body in
          List.map remove_pattern_bindings pat_lst |> ignore;
          (optimized_name, optimized_pat_lst, optimized_body)
        end else begin
          let optimized_pat_lst = List.map optimize_pattern pat_lst in
          let optimized_body = optimize_expr body in
          List.map remove_pattern_bindings pat_lst |> ignore;
          let optimized_name = add_binding name in
          (optimized_name, optimized_pat_lst, optimized_body)
        end
      in
      let optimized_in_expr = optimize_expr in_expr in
      let optimized =
        LetBinding (
          FunctionAssignment
            (optimized_name, is_rec, optimized_pat_lst, optimized_body, curry),
          optimized_in_expr
        ) in
      remove_binding name |> ignore;
      optimized
  | LetBinding (
      TailRecursiveFunctionAssignment (name, pat_lst, body), in_expr) ->
      let optimized_name = add_binding name in
      let optimized_pat_lst = List.map optimize_pattern pat_lst in
      let optimized_body = optimize_expr body in
      List.map remove_pattern_bindings pat_lst |> ignore;
      let optimized_in_expr = optimize_expr in_expr in
      let optimized =
        LetBinding (
          TailRecursiveFunctionAssignment
            (optimized_name, optimized_pat_lst, optimized_body),
          optimized_in_expr
        ) in
      remove_binding name |> ignore;
      optimized
  | MatchExpr (target, match_cases) ->
      let optimized_target = optimize_expr target in
      let optimize_match_cases = List.map (fun el ->
        match el with
          | (pat, value, Some guard) ->
              let optimized_pat = optimize_pattern pat in
              let optimized_value = optimize_expr value in
              let optimized_guard = optimize_expr guard in
              remove_pattern_bindings pat |> ignore;
              (optimized_pat, optimized_value, Some optimized_guard)
          | (pat, value, None) ->
              let optimized_pat = optimize_pattern pat in
              let optimized_value = optimize_expr value in
              remove_pattern_bindings pat |> ignore;
              (optimized_pat, optimized_value, None)
      ) match_cases in
      MatchExpr (optimized_target, optimize_match_cases)
  | PrefixOp (pref, expr) -> PrefixOp (pref, optimize_expr expr)
  | InfixOp (l_expr, op, r_expr) ->
      InfixOp (optimize_expr l_expr, op, optimize_expr r_expr)
  | Ternary (cond_expr, then_expr, Some else_expr) ->
      Ternary (optimize_expr cond_expr, optimize_expr then_expr,
        Some (optimize_expr else_expr))
  | Ternary (cond_expr, then_expr, None) ->
      Ternary (optimize_expr cond_expr, optimize_expr then_expr, None)
  | Sequential (expr1, expr2) ->
      Sequential (optimize_expr expr1, optimize_expr expr2)
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
  | Record lst ->
      Record (List.map (fun (prop, value) -> (prop, optimize_expr value)) lst)
  | Variant (constr, Some arg) -> Variant (constr, Some (optimize_expr arg))
  | PropertyAccessor (expr, prop) -> PropertyAccessor (optimize_expr expr, prop)
  | (Constant _ as tr)
  | (ModuleAccessor (_, _) as tr)
  | (Variant (_, None) as tr) -> tr

let optimize tr =
  List.map (fun module_item ->
    match module_item with
    | Expr expr ->
        Expr (optimize_expr expr)
    | LetDecl (VarAssignment (pat, is_rec, assign_expr)) ->
        let (optimized_pat, optimized_assign_expr) =
          if is_rec then begin
            let optimized_pat = optimize_pattern pat in
            let optimized_assign = optimize_expr assign_expr in
            (optimized_pat, optimized_assign)
          end else begin
            let optimized_assign = optimize_expr assign_expr in
            let optimized_pat = optimize_pattern pat in
            (optimized_pat, optimized_assign)
          end
        in
        LetDecl (
          VarAssignment (optimized_pat, is_rec, optimized_assign_expr)
        )
    | LetDecl (FunctionAssignment (name, is_rec, pat_lst, body, curry)) ->
        let (optimized_name, optimized_pat_lst, optimized_body) =
          if is_rec then begin
            let optimized_name = add_binding name in
            let optimized_pat_lst = List.map optimize_pattern pat_lst in
            let optimized_body = optimize_expr body in
            List.map remove_pattern_bindings pat_lst |> ignore;
            (optimized_name, optimized_pat_lst, optimized_body)
          end else begin
            let optimized_pat_lst = List.map optimize_pattern pat_lst in
            let optimized_body = optimize_expr body in
            let optimized_name = add_binding name in
            List.map remove_pattern_bindings pat_lst |> ignore;
            (optimized_name, optimized_pat_lst, optimized_body)
          end
        in
        LetDecl (
          FunctionAssignment
            (optimized_name, is_rec, optimized_pat_lst, optimized_body, curry)
        )
    | LetDecl (TailRecursiveFunctionAssignment (name, pat_lst, body)) ->
        let optimized_name = add_binding name in
        let optimized_pat_lst = List.map optimize_pattern pat_lst in
        let optimized_body = optimize_expr body in
        LetDecl (
          TailRecursiveFunctionAssignment
            (optimized_name, optimized_pat_lst, optimized_body)
        )
    | t -> t
  ) tr
