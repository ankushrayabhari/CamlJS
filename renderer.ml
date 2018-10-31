open Ast

let render_constant = function
  | Int v -> string_of_int v
  | EmptyList -> "[]"
  | Bool b -> string_of_bool b
  | Float f -> string_of_float f
  | StringLiteral s -> s
  | CharLiteral c -> c
  | Unit -> "null"

let rec render_let_binding = function
  | VarAssignment (pat, expr) ->
    let rendered_target =
      Printf.sprintf "let TARGET = (%s);" (render_expr expr) in
    let (bindings, constants) = get_pattern_bindings (ref 0) "TARGET" pat in
    let rendered_match_case = render_match_case bindings constants in
    Printf.sprintf "%s%s"
      rendered_target
      rendered_match_case
  | FunctionAssignment (function_name, _, arg_list, body_expr) ->
      "let " ^ (Str.global_replace (Str.regexp "'") "$" function_name) ^ " = "
      ^ render_fun arg_list body_expr ^  ";"

and render_match_case bindings constants =
  let rendered_bindings =
    List.fold_left (fun acc (target_var, target_value) ->
      acc ^ (Printf.sprintf "let %s = %s;" target_var target_value)
    ) "" bindings
  in
  let rendered_constant_bindings =
    List.fold_left (fun acc (target_var, target_value, _) ->
      acc ^ (Printf.sprintf "let %s = %s;" target_var target_value)
    ) "" constants
  in
  let rendered_binding_assertions =
    List.fold_left (fun acc (target_var, _) ->
      acc ^ Printf.sprintf
        "if (%s === undefined) { throw new Error('Match failure');}"
        target_var
    ) "" bindings
  in
  let rendered_constant_binding_assertions =
    List.fold_left (fun acc (target_var, _, expected_value) ->
      acc ^ Printf.sprintf
        "if (Pervasives.compare(%s)(%s)!==0){throw new Error('Match failure');}"
        target_var
        expected_value
    ) "" constants
  in
  Printf.sprintf "%s%s%s%s"
    rendered_bindings
    rendered_constant_bindings
    rendered_binding_assertions
    rendered_constant_binding_assertions

and render_fun arg_list body_expr =
  let arg_names = List.mapi (fun idx _ -> "A"^(string_of_int idx)) arg_list in
  let arguments = List.fold_left (fun acc el ->
      acc ^ el ^ " => "
    ) "" arg_names
  in
  let argument_match_expr =
    List.fold_right2 (fun arg_pat arg_name prev_expr ->
      MatchExpr (
        VarName arg_name,
        [(arg_pat, prev_expr, None)]
      )
    )
    arg_list
    arg_names
    body_expr
  in
  match argument_match_expr with
  | MatchExpr (target, pat_lst) ->
      "(" ^ arguments ^ (render_match_expr target pat_lst) ^ ")"
  | _ -> failwith "should be a Match Expression"

and render_prefix_op = function
  | Negation -> "-"
  | NegationFloat -> "-"

and render_ternary condition_expr true_expr false_expr =
  let rendered_condition_expr = render_expr condition_expr in
  let rendered_true_expr = render_expr true_expr in
  let rendered_false_expr = match false_expr with
    | None -> "undefined"
    | Some expr -> render_expr expr
  in
  "(( " ^ rendered_condition_expr ^ ") ? " ^
  rendered_true_expr ^ " : " ^ rendered_false_expr ^ ")"

and render_prefix_expr prefix expr =
  let rendered_op = render_prefix_op prefix in
  let rendered_expr = render_expr expr in
  "(" ^ rendered_op ^ "(" ^ rendered_expr ^ "))"

and render_infix_expr l_expr op r_expr =
  let rendered_l = render_expr l_expr in
  let rendered_r = render_expr r_expr in
  match op with
  | Plus | PlusFloat -> rendered_l ^ "+" ^ rendered_r
  | Minus | MinusFloat -> rendered_l ^ "-" ^ rendered_r
  | Divide | DivideFloat -> rendered_l ^ "/" ^ rendered_r
  | Times | TimesFloat -> rendered_l ^ "*" ^ rendered_r
  | GreaterThan -> rendered_l ^ ">" ^ rendered_r
  | LessThan -> rendered_l ^ "<" ^ rendered_r
  | GreaterThanOrEqual -> rendered_l ^ ">=" ^ rendered_r
  | LessThanOrEqual -> rendered_l ^ "<=" ^ rendered_r
  | Equal -> rendered_l ^ "===" ^ rendered_r
  | NotEqual -> rendered_l ^ "!==" ^ rendered_r
  | Cons -> "List.cons(" ^ rendered_l ^ ")(" ^ rendered_r ^ ")"
  | Append -> "List.append(" ^ rendered_l ^ ")(" ^ rendered_r ^ ")"
  | LogicalAnd -> rendered_l ^ "&&" ^ rendered_r
  | LogicalOr -> rendered_l ^ "||" ^ rendered_r
  | Concat -> rendered_l ^ "+" ^ rendered_r

and render_sequential_expr expr_1 expr_2 =
  let rendered_1 = render_expr expr_1 in
  let rendered_2 = render_expr expr_2 in
  "(" ^ rendered_1 ^ ", " ^ rendered_2 ^ ")"

and render_let_binding_expr assign expr =
  let rendered_assign = render_let_binding assign in
  let rendered_in_expr = render_expr expr in
  "(() => {" ^ rendered_assign ^ "{ return " ^ rendered_in_expr ^ "}})()"

and render_paren_expr expr =
  let rendered_expr = render_expr expr in
  "(" ^ rendered_expr ^ ")"

and render_function_call f_expr arg_expr =
  let rendered_function = render_expr f_expr in
  let rendered_argument = render_expr arg_expr in
  rendered_function ^ "(" ^ rendered_argument ^ ")"

and render_list_expr lst =
  lst
  |> List.rev
  |> List.map render_expr
  |> String.concat ","
  |> (fun lst_body -> "[" ^ lst_body ^ "]")

and render_module_accessor module_name value_name =
  module_name ^ "." ^ value_name

and get_pattern_bindings curr_bind_idx target_var pattern =
  match pattern with
  | IgnorePattern ->
    let curr_bind_name = "BINDING"^(string_of_int !curr_bind_idx) in
    incr curr_bind_idx;
    ([(curr_bind_name, target_var)], [])
  | ConstantPattern c ->
    let curr_bind_name = "BINDING"^(string_of_int !curr_bind_idx) in
    incr curr_bind_idx;
    ([], [(curr_bind_name, target_var, render_constant c)])
  | ValueNamePattern v ->
    ([(Str.global_replace (Str.regexp "'") "$" v, target_var)], [])
  | AliasPattern (pat, alias) ->
    let (bindings, constant_assertions) =
      get_pattern_bindings curr_bind_idx target_var pat in
    ((alias, target_var)::bindings, constant_assertions)
  | ParenPattern p -> get_pattern_bindings curr_bind_idx target_var p
  | ListPattern lst ->
    let pat_lst_length = List.length lst in
    let (_, bindings, constant_assertions) =
      List.fold_left (fun (idx, bindings, constant_assertions) pat ->
        let (r, c) =
          get_pattern_bindings
            curr_bind_idx
            (Printf.sprintf "%s[%d]" target_var (pat_lst_length - 1 - idx))
            pat
        in
        (idx + 1, bindings@r, constant_assertions@c)
      ) (0, [], []) lst
    in
    let (_, length_assertion) =
      get_pattern_bindings
        curr_bind_idx
        (target_var^".length")
        (ConstantPattern (Int pat_lst_length))
    in
    (bindings, (List.hd length_assertion)::constant_assertions)
  | ConsPattern (hd_pat, tl_pat) ->
    let hd_target_var = Printf.sprintf "%s.slice(-1)[0]" target_var in
    let tl_target_var = Printf.sprintf "%s.slice(0, -1)" target_var in
    let (hd_bindings, hd_constant_assertions) =
      get_pattern_bindings
        curr_bind_idx
        hd_target_var
        hd_pat
    in
    let (tl_bindings, tl_constant_assertions) =
      get_pattern_bindings
        curr_bind_idx
        tl_target_var
        tl_pat
    in
    (hd_bindings@tl_bindings, hd_constant_assertions@tl_constant_assertions)

and render_match_expr target_expr pat_lst =
  let rendered_target =
    Printf.sprintf "let TARGET = (%s);" (render_expr target_expr) in
  let rendered_match_cases = List.fold_left (fun acc (pat, expr, _) ->
    let rendered_value = render_expr expr in
    let (bindings, constants) = get_pattern_bindings (ref 0) "TARGET" pat in
    let rendered_match_case = render_match_case bindings constants in
    acc ^ Printf.sprintf "try {%sreturn %s;} catch (err) {};"
      rendered_match_case
      rendered_value
  ) "" pat_lst
  in
  Printf.sprintf "(() => {%s%sthrow new Error('Match Failure');})()"
    rendered_target
    rendered_match_cases

and render_expr = function
  | Constant c -> render_constant c
  | PrefixOp (prefix, expr) -> render_prefix_expr prefix expr
  | InfixOp (l, op, r) -> render_infix_expr l op r
  | Ternary (c, t, f) -> render_ternary c t f
  | Function (arg_list, body_expr) -> render_fun arg_list body_expr
  | Sequential (expr_1, expr_2) -> render_sequential_expr expr_1 expr_2
  | LetBinding (assign, expr) -> render_let_binding_expr assign expr
  | VarName name -> Str.global_replace (Str.regexp "'") "$" name
  | FunctionCall (expr_1, expr_2) -> render_function_call expr_1 expr_2
  | ParenExpr (expr) -> render_paren_expr expr
  | ListExpr expr_lst -> render_list_expr expr_lst
  | ModuleAccessor (m, v) -> render_module_accessor m v
  | MatchExpr (expr, lst) -> render_match_expr expr lst

let render_open_decl = function
  | "Pervasives" -> Pervasives_js.destructure
  | "List" -> List_js.destructure
  | "Char" -> Char_js.destructure
  | "String" -> String_js.destructure
  | t -> failwith ("Open Decl not supported for " ^ t)

let render_let_decl = function
  | LetDecl let_bind -> render_let_binding let_bind
  | _ -> failwith "Not a Let Decl"

let render_module_items_list lst =
  let (left, right) = List.fold_left (fun (left_body, right_body) el ->
    match el with
      | LetDecl let_bind ->
        (left_body ^ render_let_binding let_bind ^ "{", right_body ^ "}")
      | OpenDecl module_name ->
        (left_body ^ render_open_decl module_name ^ "{", right_body ^ "}")
      | Expr e ->
        (left_body ^ render_expr e ^ ";", right_body)
  ) ("", "") lst in
  left ^ right

let render ast =
  Pervasives_js.impl ^
  List_js.impl ^
  Char_js.impl ^
  String_js.impl ^
  render_module_items_list ((OpenDecl "Pervasives")::ast)
