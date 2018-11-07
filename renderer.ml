open Printf
open Ast

(**
 * [render_constant c] is the JavaScript equivalent code of declaring a constant
 * [c].
 * - [Int], [Float] both are represented by [Number].
 * - [EmptyList] is represented as an empty [Array].
 * - [Bool] is represented as a [Boolean].
 * - [StringLiteral] and [CharLiteral] are both represented as a [String] with
 * [CharLiteral] being a [String] of length 1.
 * - [Unit] is represented by [null].
 *)
let render_constant = function
  | Int v -> string_of_int v
  | EmptyList -> "[]"
  | Bool b -> string_of_bool b
  | Float f -> string_of_float f
  | StringLiteral s -> s
  | CharLiteral c -> c
  | Unit -> "null"
  | EmptyArray -> "[]"

let render_binding_assignment target_var target_value =
  sprintf "let %s = %s;" target_var target_value

let render_equality_assertion target_var expected_value =
  sprintf
    "if (Pervasives.compare(%s)(%s)!==0){throw new Error('Match failure');}"
    target_var
    expected_value

let render_defined_assertion target_var =
  sprintf
    "if (%s === undefined) { throw new Error('Match failure');}"
    target_var

let render_ranged_assertion target_var lower_bound upper_bound =
  sprintf
    "if (%s >= %s && %s <= %s){throw new Error('Match failure');}"
    target_var
    lower_bound
    target_var
    upper_bound

(**
 * [render_let_binding l] is the JavaScript equivalent code of declaring a
 * binding [l].
 * - [VarAssignment] are represented as a match expression against the
 * value with only one case.
 * - [FunctionAssignment] is represented as a variable intialized to an
 * anonymous function.
 *)
let rec render_let_binding = function
  | VarAssignment (pat, expr) ->
    let rendered_target =
      Printf.sprintf "let TARGET = (%s);" (render_expr expr) in
    let (bindings, assertions) = get_pattern_bindings (ref 0) "TARGET" pat in
    let rendered_match_case = render_match_case bindings assertions None in
    Printf.sprintf "%s%s"
      rendered_target
      rendered_match_case
  | FunctionAssignment (function_name, _, arg_list, body_expr) ->
      "let " ^ (Str.global_replace (Str.regexp "'") "$" function_name) ^ " = "
      ^ render_fun arg_list body_expr ^  ";"

(**
 * [render_match_case bindings constant] is the JavaScript equivalent code of
 * declaring a match case.
 * - [bindings] is a list of tuples where each tuple contains the target
 * varible identifier and the value it was bound to.
 * - [constant] is a list of tuples where each tuple contains the target
 * varible identifier and the value it was bound to and its expected value.
 *
 * This is translated into JavaScript by:
 * - creating a list of binding statements
 * - a list of assertions to check that none of the bound variables were
 * [undefined].
 * - A list of constant assertions to check that each constant pattern matched
 * its expected value.
 *)
and render_match_case bindings assertions guard =
  let rendered_bindings = String.concat ";" bindings in
  let rendered_assertions = String.concat ";" assertions in
  let rendered_guard_assertion = match guard with
    | Some expr -> Printf.sprintf
        "if (!(%s)) { throw new Error('Match failure');}"
        (render_expr expr)
    | None -> ""
  in
  Printf.sprintf "%s%s%s"
    rendered_bindings
    rendered_assertions
    rendered_guard_assertion

(**
 * [render_fun arg_list body_expr] is the JavaScript equivalent code of
 * declaring an anonymous function.
 * - [arg_list] is the list of patterns for each argument of the function.
 * - [body_expr] is the function body.
 *
 * This is translated into JavaScript by:
 * - Creating a function that has the arguments labelled as A0 to AN.
 * - The function is declared as a curry to match OCaml's default behavior.
 * - Immediately, pattern matching each Ai against the ith pattern.
 *)
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

(**
 * [render_prefix_op p] is the JavaScript equivalent code of [p]
 *)
and render_prefix_op = function
  | Negation -> "-"
  | NegationFloat -> "-"

(**
 * [render_ternary c t f] is the JavaScript equivalent code of declaring a
 * ternary expression that evaluates to [t] if [c] is true or [f] otherwise.
 *
 * If [f] is [None], then the code evaluates to [undefined].
 *)
and render_ternary condition_expr true_expr false_expr =
  let rendered_condition_expr = render_expr condition_expr in
  let rendered_true_expr = render_expr true_expr in
  let rendered_false_expr = match false_expr with
    | None -> "undefined"
    | Some expr -> render_expr expr
  in
  "(( " ^ rendered_condition_expr ^ ") ? " ^
  rendered_true_expr ^ " : " ^ rendered_false_expr ^ ")"

(**
 * [render_prefix_expr prefix expr] is the JavaScript equivalent code of
 * declaring an expression that applies operator [prefix] to [expr].
 *)
and render_prefix_expr prefix expr =
  let rendered_op = render_prefix_op prefix in
  let rendered_expr = render_expr expr in
  "(" ^ rendered_op ^ "(" ^ rendered_expr ^ "))"

(**
 * [render_infix_expr l_expr op r_expr] is the JavaScript equivalent code of
 * declaring an infix expression that applies operator [op] to a left
 * expression [l_expr] and a right expression [r_expr].
 *)
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

(**
 * [render_sequential_expr expr1 expr2] is the JavaScript equivalent code of
 * declaring an expression that executes [expr1] and returns the value of
 * [expr2].
 *
 * This is implemented using the comma operator in JavaScript.
 *)
and render_sequential_expr expr_1 expr_2 =
  let rendered_1 = render_expr expr_1 in
  let rendered_2 = render_expr expr_2 in
  "(" ^ rendered_1 ^ ", " ^ rendered_2 ^ ")"

(**
 * [render_let_binding_expr assign expr] is the JavaScript equivalent code of
 * declaring an let_binding expression that creates a binding [assign] in
 * [expr].
 *
 * This is done in JavaScript by:
 * - creating an anonymous function so there is a new lexical scope.
 * - adding all of the bindings in [assign] to the newly created scope
 * - creating a new lexical scope within which the value of [expr] is
 * returned.
 *)
and render_let_binding_expr assign expr =
  let rendered_assign = render_let_binding assign in
  let rendered_in_expr = render_expr expr in
  "(() => {" ^ rendered_assign ^ "{ return " ^ rendered_in_expr ^ "}})()"

(**
 * [render_paren_expr expr] is the JavaScript equivalent code of
 * wrapping [expr] in parentheses.
 *)
and render_paren_expr expr =
  let rendered_expr = render_expr expr in
  "(" ^ rendered_expr ^ ")"

(**
 * [render_function_call f_expr arg_expr] is the JavaScript equivalent code of
 * calling [f_expr] with [arg_expr].
 *)
and render_function_call f_expr arg_expr =
  let rendered_function = render_expr f_expr in
  let rendered_argument = render_expr arg_expr in
  rendered_function ^ "(" ^ rendered_argument ^ ")"

(**
 * [render_list_expr lst] is the JavaScript equivalent code of declaring a
 * list with elements [lst].
 *
 * This is done in JavaScript by creating an array with the [lst] values in
 * reverse.
 *)
and render_list_expr lst =
  lst
  |> List.rev
  |> List.map render_expr
  |> String.concat ","
  |> (fun lst_body -> "[" ^ lst_body ^ "]")

and render_array_tuple_expr lst =
  render_list_expr (List.rev lst)

(**
 * [render_module_accessor module value] is the JavaScript equivalent code of
 * accessing the property [value] in a object [module].
 *
 * Modules are represented by objects in JavaScript.
 *)
and render_module_accessor module_name value_name =
  module_name ^ "." ^ value_name

(**
 * [get_pattern_bindings curr_bind_idx target_var pattern] is the JavaScript
 * equivalent code getting the bindings and assertions involved with
 * [pattern].
 *
 * [target_var] is the value being matched and [curr_bind_idx] is a ref
 * containing the next index for a binding.
 *)
and get_pattern_bindings curr_bind_idx target_var pattern =
  match pattern with
  | RangedCharacterPattern (start_char, end_char) ->
      let curr_bind_name = "BINDING"^(string_of_int !curr_bind_idx) in
      ([render_binding_assignment curr_bind_name target_var], [
        render_defined_assertion curr_bind_name;
        render_ranged_assertion curr_bind_name
          (sprintf "Char.code(%s)" start_char)
          (sprintf "Char.code(%s)" end_char)
      ])
  | IgnorePattern ->
      let curr_bind_name = "BINDING"^(string_of_int !curr_bind_idx) in
      incr curr_bind_idx;
      ([render_binding_assignment curr_bind_name target_var], [
        render_defined_assertion curr_bind_name
      ])
  | ConstantPattern c ->
      let curr_bind_name = "BINDING"^(string_of_int !curr_bind_idx) in
      incr curr_bind_idx;
      ([render_binding_assignment curr_bind_name target_var], [
        render_defined_assertion curr_bind_name;
        render_equality_assertion curr_bind_name (render_constant c)
      ])
  | ValueNamePattern v ->
      let curr_bind_name = Str.global_replace (Str.regexp "'") "$" v in
      ([render_binding_assignment curr_bind_name target_var], [
        render_defined_assertion curr_bind_name
      ])
  | AliasPattern (pat, alias) ->
      let (bindings, constant_assertions) =
        get_pattern_bindings curr_bind_idx target_var pat in
      ((render_binding_assignment alias target_var)::bindings,
       (render_defined_assertion alias)::constant_assertions)
  | ParenPattern p -> get_pattern_bindings curr_bind_idx target_var p
  | (ArrayPattern lst as pat)
  | (TuplePattern lst as pat)
  | (ListPattern lst as pat) -> begin
      let pat_lst_length = List.length lst in
      let (_, bindings, constant_assertions) =
        List.fold_left (fun (idx, bindings, constant_assertions) pat ->
          let curr_idx = match pat with
            | ArrayPattern _ | TuplePattern _ -> idx
            | ListPattern _ -> pat_lst_length - 1 - idx
            | _ -> failwith "should not be anything by array/list pattern"
          in
          let (r, c) =
            get_pattern_bindings
              curr_bind_idx
              (Printf.sprintf "%s[%d]" target_var curr_idx)
              pat
          in
          (idx + 1, bindings@r, constant_assertions@c)
        ) (0, [], []) lst
      in
      let (length_binding, length_assertion) =
        get_pattern_bindings
          curr_bind_idx
          (target_var^".length")
          (ConstantPattern (Int pat_lst_length))
      in
      (length_binding@bindings, length_assertion@constant_assertions)
  end
  | ConsPattern (hd_pat, tl_pat) ->
      let hd_target_var = Printf.sprintf "(%s.slice(-1)[0])" target_var in
      let tl_target_var = Printf.sprintf "(%s.slice(0, -1))" target_var in
      let (hd_bindings, hd_assertions) =
        get_pattern_bindings
          curr_bind_idx
          hd_target_var
          hd_pat
      in
      let (tl_bindings, tl_assertions) =
        get_pattern_bindings
          curr_bind_idx
          tl_target_var
          tl_pat
      in
      (hd_bindings@tl_bindings, hd_assertions@tl_assertions)
  | RecordPattern pat_lst ->
      List.fold_left (fun (bindings, assertions) (prop, pat) ->
        let prop_target_var = Printf.sprintf "%s.%s" target_var prop in
        let (prop_bindings, prop_assertions) =
          get_pattern_bindings
            curr_bind_idx
            prop_target_var
            pat
        in
        (prop_bindings@bindings, prop_assertions@assertions)
      ) ([], []) pat_lst
  | VariantPattern (name, data_pat) ->
      let (name_binding, name_assertion) =
        get_pattern_bindings
          curr_bind_idx
          (target_var^".$NAME")
          (ConstantPattern (StringLiteral name))
      in
      let (data_binding, data_assertion) = match data_pat with
        | None -> ([], [])
        | Some pat ->
          get_pattern_bindings
            curr_bind_idx
            (target_var^".$DATA")
            pat
      in
      (name_binding@data_binding, name_assertion@data_assertion)

(**
 * [render_match_expr target_expr pat_lst] is the JavaScript
 * equivalent code of generating a match expression on [target_expr] with
 * the list of match cases [pat_lst].
 *
 * This is done in JavaScript by:
 * - create an anonymous function so there is a new lexical scope.
 * - Bind the value of [target_expr] to a new variable "TARGET" in that scope.
 * - For each match case, create a try block (new lexical scope) that contains
 * {ul
 * {li the bindings associated with that match case}
 * {li the assertions associated with that match case, that throw a
 * Match_failure error if not true}
 * {li a return in the overall expression with the value of the case's value}
 * }
 * - A catch statement in each match case that does nothing.
 * - A final throw statement that throws a match error if none of the try
 * statements return.
 *)
and render_match_expr target_expr pat_lst =
  let rendered_target =
    Printf.sprintf "let TARGET = (%s);" (render_expr target_expr) in
  let rendered_match_cases = List.fold_left (fun acc (pat, expr, guard) ->
    let rendered_value = render_expr expr in
    let (bindings, assertions) = get_pattern_bindings (ref 0) "TARGET" pat in
    let rendered_match_case = render_match_case bindings assertions guard in
    acc ^ Printf.sprintf "try {%sreturn %s;} catch (err) {};"
      rendered_match_case
      rendered_value
  ) "" pat_lst
  in
  Printf.sprintf "(() => {%s%sthrow new Error('Match Failure');})()"
    rendered_target
    rendered_match_cases

and render_record_expr prop_lst =
  List.map (fun (prop, val_expr) ->
    sprintf "%s: %s" prop (render_expr val_expr)
  ) prop_lst
  |> String.concat ","
  |> sprintf "{%s}"

and render_variant_expr name arg_expr =
  let rendered_arg = match arg_expr with
    | Some arg -> render_expr arg
    | None -> "null"
  in
  sprintf "{$NAME: \"%s\", $DATA: %s}" name rendered_arg

(**
 * [render_expr e] is the JavaScript equivalent code of [e]. This function
 * simply calls all the previous expression rendering functions.
 *)
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
  | ArrayExpr expr_lst | Tuple expr_lst -> render_array_tuple_expr expr_lst
  | Record prop_lst -> render_record_expr prop_lst
  | Variant (name, arg_expr) -> render_variant_expr name arg_expr

(**
 * [render_open_decl m] is the JavaScript equivalent code of bringing all of
 * [m]'s bindings into the current scope.
 * @raise Failure if [m] does not have a JS port.
 *)
let render_open_decl = function
  | "Pervasives" -> Pervasives_js.destructure
  | "List" -> List_js.destructure
  | "Char" -> Char_js.destructure
  | "String" -> String_js.destructure
  | t -> failwith ("Open Decl not supported for " ^ t)

(**
 * [render_module_items_list lst] is the JavaScript equivalent code of rendering
 * each module item in [lst] in order.
 *
 * {b See:} [render_let_binding], [render_open_decl], [render_expr]
 *)
let render_module_items_list lst =
  let (left, right) = List.fold_left (fun (left_body, right_body) el ->
    match el with
      | LetDecl let_bind ->
        (left_body ^ render_let_binding let_bind ^ "{", right_body ^ "}")
      | OpenDecl module_name ->
        (left_body ^ render_open_decl module_name ^ "{", right_body ^ "}")
      | Expr e ->
        (left_body ^ render_expr e ^ ";", right_body)
      | TypeDefinition _ -> (left_body, right_body)
  ) ("", "") lst in
  left ^ right

let render ast =
  Pervasives_js.impl ^
  List_js.impl ^
  Char_js.impl ^
  String_js.impl ^
  render_module_items_list ((OpenDecl "Pervasives")::ast)
