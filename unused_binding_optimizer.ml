open Ast

(**
 * [unused_variables] is the set of unused variables in the program.
 *)
let unused_variables = Hashtbl.create 10000;;

(**
 * [pattern_binding_iter f tr] applies [f] to every binding name string in [tr].
 *)
let rec pattern_binding_iter f = function
  | AliasPattern (pat, alias) ->
      f alias;
      pattern_binding_iter f pat
  | ValueNamePattern str ->
      f str
  | ParenPattern pat -> pattern_binding_iter f pat
  | ListPattern pat_lst
  | TuplePattern pat_lst
  | ArrayPattern pat_lst -> List.iter (pattern_binding_iter f) pat_lst
  | ConsPattern (pat1, pat2) ->
      pattern_binding_iter f pat1;
      pattern_binding_iter f pat2
  | VariantPattern (_, Some pat) ->
      pattern_binding_iter f pat;
  | RecordPattern param_lst ->
      List.iter (fun (_, pat) ->
        pattern_binding_iter f pat
      ) param_lst
  | IgnorePattern
  | RangedCharacterPattern (_, _)
  | ConstantPattern _
  | VariantPattern (_, None) -> ()

(**
 * [add_binding name] adds [name] to unused_variables.
 *)
let add_binding name =
  Hashtbl.add unused_variables name ()

(**
 * [remove_binding name] removes [name] from unused_variables.
 *)
let remove_binding name =
  Hashtbl.remove unused_variables name

(**
 * [add_pattern_bindings tr] adds all pattern bindings in [tr] to
 * unused_variables.
 *)
let add_pattern_bindings = pattern_binding_iter add_binding

(**
 * [populate_unused_variables_let_binding tr] populates [unused_variables] with
 * any unused variable in [tr].
 *)
let rec populate_unused_variables_let_binding = function
  | VarAssignment (pat, is_rec, assign_expr) ->
      add_pattern_bindings pat;
      populate_unused_variables assign_expr;
  | FunctionAssignment (name, is_rec, pat_lst, body, curry) ->
      add_binding name;
      List.iter add_pattern_bindings pat_lst;
      populate_unused_variables body;
  | TailRecursiveFunctionAssignment (name, pat_lst, body) ->
      add_binding name;
      List.iter add_pattern_bindings pat_lst;
      populate_unused_variables body;

(**
 * [populate_unused_variables tr] populates [unused_variables] with any unused
 * variable in [tr].
 *)
and populate_unused_variables = function
  | VarName str ->
      remove_binding str
  | Function (pat_lst, body_expr) ->
      List.iter add_pattern_bindings pat_lst;
      populate_unused_variables body_expr
  | LetBinding (let_bind, in_expr) ->
      populate_unused_variables_let_binding let_bind;
      populate_unused_variables in_expr
  | MatchExpr (target, match_cases) ->
      populate_unused_variables target;
      List.iter (fun el ->
        match el with
          | (pat, value, Some guard) ->
              add_pattern_bindings pat;
              populate_unused_variables value;
              populate_unused_variables guard
          | (pat, value, None) ->
              add_pattern_bindings pat;
              populate_unused_variables value
      ) match_cases
  | PrefixOp (_, expr) -> populate_unused_variables expr
  | InfixOp (l_expr, _, r_expr) ->
      populate_unused_variables l_expr;
      populate_unused_variables r_expr
  | Ternary (cond_expr, then_expr, Some else_expr) ->
      populate_unused_variables cond_expr;
      populate_unused_variables then_expr;
      populate_unused_variables else_expr
  | Ternary (cond_expr, then_expr, None) ->
      populate_unused_variables cond_expr;
      populate_unused_variables then_expr
  | Sequential (expr1, expr2) ->
      populate_unused_variables expr1;
      populate_unused_variables expr2
  | FunctionCall (fun_expr, arg_lst, curried) ->
      List.iter populate_unused_variables arg_lst;
      populate_unused_variables fun_expr
  | ParenExpr expr -> populate_unused_variables expr
  | ListExpr lst
  | ArrayExpr lst
  | Tuple lst -> List.iter populate_unused_variables lst
  | Record lst ->
      List.iter (fun (_, value) -> populate_unused_variables value) lst
  | Variant (_, Some arg) -> populate_unused_variables arg
  | PropertyAccessor (expr, _) -> populate_unused_variables expr
  | Constant _
  | ModuleAccessor (_, _)
  | Variant (_, None) -> ()

(**
 * [prune_let_decl tr] prunes any let declaration that is unused:
 * - any [VarAssignment] that only contains a single [ValueNamePattern] is
 * pruned if unused.
 * - any [FunctionAssignment] or [TailRecursiveFunctionAssignment] is pruned
 * if they are unused.
 *)
let rec prune_let_decl = function
  | VarAssignment (pat, is_rec, assign_expr) -> begin
      match pat with
      | ValueNamePattern str when Hashtbl.mem unused_variables str -> Expr (prune_expr assign_expr)
      | _ -> LetDecl (VarAssignment (pat, is_rec, prune_expr assign_expr))
    end
  | FunctionAssignment (name, is_rec, pat_lst, body, curry) -> begin
      if Hashtbl.mem unused_variables name then Expr (Constant (Int 1))
      else LetDecl (FunctionAssignment (name, is_rec, pat_lst, prune_expr body, curry))
    end
  | TailRecursiveFunctionAssignment (name, pat_lst, body) -> begin
      if Hashtbl.mem unused_variables name then Expr (Constant (Int 1))
      else LetDecl (TailRecursiveFunctionAssignment (name, pat_lst, prune_expr body))
    end

(**
 * [prune_expr tr] prunes any let declaration that is unused:
 * - any [VarAssignment] that only contains a single [ValueNamePattern] is
 * pruned if unused.
 * - any [FunctionAssignment] or [TailRecursiveFunctionAssignment] is pruned
 * if they are unused.
 *)
and prune_expr = function
  | LetBinding (let_bind, in_expr) -> begin
      match prune_let_decl let_bind with
      | Expr _ -> prune_expr in_expr
      | LetDecl d -> LetBinding (d, prune_expr in_expr)
      | _ -> failwith "should not be anything else"
  end
  | (Constant _ as tr)
  | (VarName _ as tr)
  | (ModuleAccessor (_, _) as tr)
  | (Variant (_, None) as tr) -> tr
  | PrefixOp (pref, expr) -> PrefixOp (pref, prune_expr expr)
  | InfixOp (l_expr, op, r_expr) ->
      InfixOp (prune_expr l_expr, op, prune_expr r_expr)
  | Ternary (cond_expr, then_expr, Some else_expr) ->
      Ternary (prune_expr cond_expr, prune_expr then_expr,
        Some (prune_expr else_expr))
  | Ternary (cond_expr, then_expr, None) ->
      Ternary (prune_expr cond_expr, prune_expr then_expr, None)
  | Function (pat_lst, body_expr) ->
      Function (pat_lst, prune_expr body_expr)
  | Sequential (expr1, expr2) ->
      Sequential (prune_expr expr1, prune_expr expr2)
  | FunctionCall (fun_expr, arg_lst, curried) ->
      FunctionCall (
        prune_expr fun_expr,
        List.map prune_expr arg_lst,
        curried
      )
  | ParenExpr expr -> ParenExpr (prune_expr expr)
  | ListExpr lst -> ListExpr (List.map prune_expr lst)
  | ArrayExpr lst -> ArrayExpr (List.map prune_expr lst)
  | Tuple lst -> Tuple (List.map prune_expr lst)
  | MatchExpr (target_expr, pat_lst) ->
      MatchExpr (
        prune_expr target_expr,
        List.map (fun el -> match el with
          | (pat, value, Some guard) ->
              (pat, prune_expr value, Some (prune_expr guard))
          | (pat, value, None) -> (pat, prune_expr value, None)
        ) pat_lst
      )
  | Record lst ->
      Record (List.map (fun (name, value) -> (name, prune_expr value)) lst)
  | Variant (constr, Some arg) -> Variant (constr, Some (prune_expr arg))
  | PropertyAccessor (expr, prop) -> PropertyAccessor (prune_expr expr, prop)

let optimize tr =
  List.iter (fun mod_item -> match mod_item with
    | LetDecl let_bind -> populate_unused_variables_let_binding let_bind
    | Expr e -> populate_unused_variables e
    | _ -> ()
  ) tr;
  List.map (fun mod_item -> match mod_item with
    | LetDecl let_bind -> prune_let_decl let_bind
    | Expr e -> Expr (prune_expr e)
    | t -> t
  ) tr
