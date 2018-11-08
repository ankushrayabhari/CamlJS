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

let rec if_expr_curr name num_args expr : bool =
  match expr with
  | PrefixOp (_, ex) -> (if_expr_curr name num_args ex)
  | InfixOp (ex1, _, ex2) -> 
    (if_expr_curr name num_args ex1) || 
    (if_expr_curr name num_args ex2)
  | Ternary (ex1, ex2, Some ex3) -> 
    (if_expr_curr name num_args ex1) || 
    (if_expr_curr name num_args ex2) || 
    (if_expr_curr name num_args ex3)
  | Ternary (ex1, ex2, None) -> 
    (if_expr_curr name num_args ex1) || 
    (if_expr_curr name num_args ex2)
  | Function (plist, ex) -> 
    if (List.fold_left (fun acc p -> (name_bound_in_pat name p) || acc) 
    false plist) then false else (if_expr_curr name num_args ex)
  | Sequential (ex1, ex2) -> 
    (if_expr_curr name num_args ex1) || 
    (if_expr_curr name num_args ex2)
  | LetBinding (VarAssignment (pat, val_ex), in_ex) -> 
    if (name_bound_in_pat name pat) 
    then (if_expr_curr name num_args val_ex) 
    else (if_expr_curr name num_args val_ex) || 
        (if_expr_curr name num_args in_ex)
  | LetBinding (FunctionAssignment (n, recr, pat_lst, fun_ex, _), in_ex) -> 
    if (n=name) || 
      (List.fold_left (fun acc p -> (name_bound_in_pat name p) || acc) 
      false pat_lst) 
    then if (n=name) && recr then false else (if_expr_curr name num_args fun_ex) 
    else (if_expr_curr name num_args fun_ex) || 
        (if_expr_curr name num_args in_ex)
  | FunctionCall (ex, ex_list, _) -> 
    begin match ex with
      | VarName s -> if (s=name) then ((List.length ex_list)<>num_args) else false
      | _ -> if_expr_curr name num_args ex
    end
  | ParenExpr ex -> (if_expr_curr name num_args ex)
  | ListExpr (ex_list) -> List.fold_left (fun acc e -> (if_expr_curr name num_args e) || acc) false ex_list
  | ArrayExpr (ex_list) -> List.fold_left (fun acc e -> (if_expr_curr name num_args e) || acc) false ex_list
  | MatchExpr (ex, pat_ex_exop_lst) -> (if_expr_curr name num_args ex) || (List.fold_left (fun acc (p, e, eo) -> if (name_bound_in_pat name p) then false || acc else (if_expr_curr name num_args e)||(match eo with | Some o -> if_expr_curr name num_args o | None -> false)||acc) false pat_ex_exop_lst)
  | Tuple (ex_list) -> List.fold_left (fun acc e -> (if_expr_curr name num_args e) || acc) false ex_list
  | Record (str_ex_list) -> List.fold_left (fun acc (_,e) -> (if_expr_curr name num_args e) || acc) false str_ex_list
  | Variant (str, Some ex) -> if_expr_curr name num_args ex
  | VarName s -> true
  | _ -> false

let rec uncurry_expr name expr =
  match expr with
  | PrefixOp (p, ex) -> PrefixOp (p, uncurry_expr name ex)
  | InfixOp (ex1, i, ex2) -> 
    InfixOp (uncurry_expr name ex1, i, uncurry_expr name ex2)
  | Ternary (ex1, ex2, Some ex3) -> 
    Ternary (uncurry_expr name ex1, uncurry_expr name ex2, Some (uncurry_expr name ex3))
  | Ternary (ex1, ex2, None) -> 
    Ternary (uncurry_expr name ex1, uncurry_expr name ex2, None)
  | Function (plist, ex) -> Function (plist, uncurry_expr name ex)
  | Sequential (ex1, ex2) -> Sequential (uncurry_expr name ex1, uncurry_expr name ex2)
  | LetBinding (VarAssignment (pat, val_ex), in_ex) -> 
    LetBinding (VarAssignment (pat, uncurry_expr name val_ex), uncurry_expr name in_ex)
  | LetBinding (FunctionAssignment (n, recr, pat_lst, fun_ex, b), in_ex) -> 
    (let inner_fun_assign = optimize [LetDecl (FunctionAssignment (n, recr, pat_lst, fun_ex, b))] in
    begin match inner_fun_assign with
    | (LetDecl l)::t -> LetBinding (l, uncurry_expr n in_ex)
    | _ -> failwith "optimization did not work"
    end)
  | FunctionCall (ex, ex_list, b) as f -> 
    begin match ex with
      | VarName s -> if (s=name) then FunctionCall (ex, ex_list, false) else f
      | _ -> uncurry_expr name ex
    end
  | ParenExpr ex -> uncurry_expr name ex
  | ListExpr (ex_list) -> ListExpr (List.map (fun e -> uncurry_expr name e) ex_list)
  | ArrayExpr (ex_list) -> ListExpr (List.map (fun e -> uncurry_expr name e) ex_list)
  | MatchExpr (ex, pat_ex_exop_lst) -> 
    MatchExpr (uncurry_expr name ex, 
    List.map (fun (p, e, eo) -> if (name_bound_in_pat name p) then (p, e, eo) else match eo with | Some o -> (p, uncurry_expr name e, Some (uncurry_expr name o)) | None -> (p, uncurry_expr name e, None)) pat_ex_exop_lst)
  | Tuple (ex_list) -> Tuple (List.map (fun e -> uncurry_expr name e) ex_list)
  | Record (str_ex_list) -> Record (List.map (fun (s,e) -> (s, uncurry_expr name e)) str_ex_list)
  | Variant (str, Some ex) -> Variant (str, Some (uncurry_expr name ex))
  | _ -> expr

and uncurry name tr =
match tr with
| first::rest -> begin
  match first with
  | Expr e -> (Expr (uncurry_expr name e))::(uncurry name rest)
  | _ -> first::(uncurry name rest)
  end
| [] -> []

and check_for_curry name num_args tr =
match tr with
| first::rest -> begin
  match first with
  | Expr e -> (if_expr_curr name num_args e) || check_for_curry name num_args rest
  | _ -> check_for_curry name num_args rest
  end
| [] -> false

and optimize tr =
match tr with
| first::rest -> 
  begin match first with 
  | LetDecl (FunctionAssignment (name, recur, lst, expr, curry)) -> 
    let should_curry = check_for_curry name (List.length lst) rest in
    (LetDecl (FunctionAssignment (name, recur, lst, expr, should_curry)))::
    (if should_curry then optimize rest else optimize (uncurry name rest))
  | _ -> optimize rest
end
| [] -> tr
