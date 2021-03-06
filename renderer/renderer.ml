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

(**
 * [render_binding_assignment reassign target_var target_value] is the
 * JavaScript equivalent code of assigining a variable [target_var] to
 * [target_value].
 *
 * The variable is declared if [reassign] is false.
 *)
let render_binding_assignment reassign target_var target_value =
  let reassign = if reassign then ""  else "let " in
  sprintf "%s%s = %s;" reassign target_var target_value

(**
 * [render_equality_assertion target_var expected_value] is the
 * JavaScript equivalent code of asserting that [target_var] is equal to
 * [expected_value].
 *
 * A [Match_failure] is thrown in the generated code if they are not equal.
 *)
let render_equality_assertion target_var expected_value =
  sprintf
    "if (Pervasives.compare(%s)(%s)!==0){throw new Error('Match failure');}"
    target_var
    expected_value

(**
 * [render_defined_assertion target_var] is the JavaScript equivalent code of
 * asserting that [target_var] is not [undefined].
 *
 * A [Match_failure] is thrown in the generated code if it is undefined.
 *)
let render_defined_assertion target_var =
  sprintf
    "if (%s === undefined) { throw new Error('Match failure');}"
    target_var

(**
 * [render_ranged_assertion target_var l u] is the JavaScript equivalent code of
 * asserting that [target_var] is within the range [[l, u]].
 *
 * This is inclusive of both [l] and [u],
 *
 * A [Match_failure] is thrown in the generated code if it is undefined.
 *)
let render_ranged_assertion target_var lower_bound upper_bound =
  sprintf
    "if (%s < %s || %s > %s){throw new Error('Match failure');}"
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
  | VarAssignment (pat, _, expr) ->
    let rendered_target =
      Printf.sprintf "let TARGET = (%s);" (render_expr expr) in
    let (bindings, assertions) = 
      get_pattern_bindings false (ref 0) "TARGET" pat in
    let rendered_match_case = render_match_case bindings assertions None in
    Printf.sprintf "%s%s"
      rendered_target
      rendered_match_case
  | FunctionAssignment (function_name, _, arg_list, body_expr, curry) ->
      "let " ^ (Str.global_replace (Str.regexp "'") "$" function_name) ^ " = "
      ^ render_fun arg_list body_expr curry ^  ";"
  | TailRecursiveFunctionAssignment (function_name, arg_list, body_expr) ->
      "let " ^ (Str.global_replace (Str.regexp "'") "$" function_name) ^ " = "
      ^ render_tail_rec_fn function_name arg_list body_expr ^  ";"

(**
 * [render_tail_rec_fn_body_expr name tr] is the JavaScript equivalent code of
 * the body ([tr]) of a tail recursive function named [name].
 *
 * This is implemented in JavaScript as a loop where
 * - the first thing the loop does is pattern match against the arguments
 * - On a tail call, the loop executes again with the given arguments.
 * - On reaching a value, the value is returned from the function.
 *)
and render_tail_rec_fn_body_expr name tr = match tr with
  | Ternary (cond, then_body_expr, Some else_body_expr) ->
      sprintf "if (%s) {%s} else {%s}"
        (render_expr cond)
        (render_tail_rec_fn_body_expr name then_body_expr)
        (render_tail_rec_fn_body_expr name else_body_expr)
  | MatchExpr (target_expr, pat_lst) ->
      let rendered_target =
        Printf.sprintf "let TARGET = (%s);" (render_expr target_expr) in
      let rendered_match_cases = List.fold_left (fun acc (pat, expr, guard) ->
        let rendered_value = render_tail_rec_fn_body_expr name expr in
        let (bindings, assertions) = 
          get_pattern_bindings false (ref 0) "TARGET" pat in
        let rendered_match_case = render_match_case bindings assertions guard in
        acc ^ Printf.sprintf "try {%s %s;} catch (err) {};"
          rendered_match_case
          rendered_value
      ) "" pat_lst
      in
      Printf.sprintf "{%s%sthrow new Error('Match Failure');}"
        rendered_target
        rendered_match_cases
  | FunctionCall (VarName name, arg_lst, false) ->
      arg_lst
      |> List.mapi (fun idx arg_expr ->
        let arg_name = "A"^(string_of_int idx) in
        render_binding_assignment true arg_name (render_expr arg_expr)
      )
      |> String.concat ""
      |> (fun x -> x ^ "continue;")
  | ParenExpr p -> "{" ^ render_tail_rec_fn_body_expr name p ^ "}"
  | Sequential (l, r) ->
      sprintf "%s;%s"
      (render_expr l)
      (render_tail_rec_fn_body_expr name r)
  | tr -> sprintf "return %s;" (render_expr tr)

(**
 * [render_tail_rec_fn name args tr] is the JavaScript equivalent code of
 * declaring a tail recursive function [name] with arguments [args] and body
 * [tr].
 *
 * This is implemented in JavaScript as a non curried function that uses
 * O(1) stack space.
 *
 * {b See:} [render_tail_rec_fn_body_expr]
 *)
and render_tail_rec_fn function_name arg_lst body_expr =
  let arg_names = List.mapi (fun idx _ -> "A"^(string_of_int idx)) arg_lst in
  let arguments = String.concat "," arg_names in
  let argument_match_expr =
    List.fold_right2 (fun arg_pat arg_name prev_expr ->
      MatchExpr (
        VarName arg_name,
        [(arg_pat, prev_expr, None)]
      )
    )
    arg_lst
    arg_names
    body_expr
  in
  let rendered_body_expr = 
    render_tail_rec_fn_body_expr function_name argument_match_expr in
  sprintf "((%s) => { while(true){%s} })" arguments rendered_body_expr

(**
 * [render_match_case bindings constant guard] is the JavaScript equivalent code
 * of declaring a match case.
 * - [bindings] is a list of tuples where each tuple contains the target
 * varible identifier and the value it was bound to.
 * - [constant] is a list of tuples where each tuple contains the target
 * varible identifier and the value it was bound to and its expected value.
 * - [guard] is the condition that guards the match.
 *
 * This is translated into JavaScript by:
 * - creating a list of binding statements
 * - an assertion to check the truthiness of the guard
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
 * [render_fun arg_list body_expr curry] is the JavaScript equivalent code of
 * declaring an anonymous function.
 * - [arg_list] is the list of patterns for each argument of the function.
 * - [body_expr] is the function body.
 * - [curry] is whether or not the function should be curried.
 *
 * This is translated into JavaScript by:
 * - Creating a function that has the arguments labelled as A0 to AN.
 * - Immediately, pattern matching each Ai against the ith pattern.
 * - Executing the code for [body_expr]
 *)
and render_fun arg_list body_expr curry =
  let arg_names = List.mapi (fun idx _ -> "A"^(string_of_int idx)) arg_list in
  let arguments =
    if curry then
      List.fold_left (fun acc el ->
        acc ^ el ^ " => "
      ) "" arg_names
    else
      "(" ^ String.concat "," arg_names ^ ") =>"
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
and render_function_call f_expr args curry =
  let rendered_function = render_expr f_expr in
  let rendered_argument =
    List.map render_expr args
    |> (fun lst ->
        if not curry then "(" ^ String.concat "," lst ^ ")"
        else
          List.map (fun el -> "(" ^ el ^ ")") lst
          |> String.concat ""
      )
  in
  rendered_function ^ rendered_argument

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
 * [render_accessor module value] is the JavaScript equivalent code of
 * accessing the property [value] in a object [module].
 *
 * Modules are represented by objects in JavaScript.
 *)
and render_accessor module_name value_name =
  module_name ^ "." ^ value_name

(**
 * [get_pattern_bindings curr_bind_idx target_var pattern] is the JavaScript
 * equivalent code getting the bindings and assertions involved with
 * [pattern].
 *
 * [target_var] is the value being matched and [curr_bind_idx] is a ref
 * containing the next index for a binding.
 *)
and get_pattern_bindings reassign curr_bind_idx target_var pattern =
  match pattern with
  | RangedCharacterPattern (start_char, end_char) ->
      let curr_bind_name = "BINDING"^(string_of_int !curr_bind_idx) in
      ([render_binding_assignment reassign curr_bind_name target_var], [
        render_defined_assertion curr_bind_name;
        render_ranged_assertion curr_bind_name
          (sprintf "Char.code(%s)" start_char)
          (sprintf "Char.code(%s)" end_char)
      ])
  | IgnorePattern ->
      let curr_bind_name = "BINDING"^(string_of_int !curr_bind_idx) in
      incr curr_bind_idx;
      ([render_binding_assignment reassign curr_bind_name target_var], [
        render_defined_assertion curr_bind_name
      ])
  | ConstantPattern c ->
      let curr_bind_name = "BINDING"^(string_of_int !curr_bind_idx) in
      incr curr_bind_idx;
      ([render_binding_assignment reassign curr_bind_name target_var], [
        render_defined_assertion curr_bind_name;
        render_equality_assertion curr_bind_name (render_constant c)
      ])
  | ValueNamePattern v ->
      let curr_bind_name = Str.global_replace (Str.regexp "'") "$" v in
      ([render_binding_assignment reassign curr_bind_name target_var], [
        render_defined_assertion curr_bind_name
      ])
  | AliasPattern (pat, alias) ->
      let (bindings, constant_assertions) =
        get_pattern_bindings reassign curr_bind_idx target_var pat in
      ((render_binding_assignment reassign alias target_var)::bindings,
       (render_defined_assertion alias)::constant_assertions)
  | ParenPattern p -> get_pattern_bindings reassign curr_bind_idx target_var p
  | (ArrayPattern lst as pat)
  | (TuplePattern lst as pat)
  | (ListPattern lst as pat) -> begin
      let pat_lst_length = List.length lst in
      let (_, bindings, constant_assertions) =
        List.fold_left (fun (idx, bindings, constant_assertions) curr ->
          let curr_idx = match pat with
            | ArrayPattern _ | TuplePattern _ -> idx
            | ListPattern _ -> pat_lst_length - 1 - idx
            | _ -> failwith "should not be anything by array/list pattern"
          in
          let (r, c) =
            get_pattern_bindings
              reassign
              curr_bind_idx
              (Printf.sprintf "%s[%d]" target_var curr_idx)
              curr
          in
          (idx + 1, bindings@r, constant_assertions@c)
        ) (0, [], []) lst
      in
      let (length_binding, length_assertion) =
        get_pattern_bindings
          reassign
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
          reassign
          curr_bind_idx
          hd_target_var
          hd_pat
      in
      let (tl_bindings, tl_assertions) =
        get_pattern_bindings
          reassign
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
            reassign
            curr_bind_idx
            prop_target_var
            pat
        in
        (prop_bindings@bindings, prop_assertions@assertions)
      ) ([], []) pat_lst
  | VariantPattern (name, data_pat) ->
      let (name_binding, name_assertion) =
        get_pattern_bindings
          reassign
          curr_bind_idx
          (target_var^".$NAME")
          (ConstantPattern (StringLiteral ("\"" ^ name ^ "\"")))
      in
      let (data_binding, data_assertion) = match data_pat with
        | None -> ([], [])
        | Some pat ->
          get_pattern_bindings
            reassign
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
    let (bindings, assertions) = 
      get_pattern_bindings false (ref 0) "TARGET" pat in
    let rendered_match_case = render_match_case bindings assertions guard in
    acc ^ Printf.sprintf "try {%sreturn %s;} catch (err) {};"
      rendered_match_case
      rendered_value
  ) "" pat_lst
  in
  Printf.sprintf "(() => {%s%sthrow new Error('Match Failure');})()"
    rendered_target
    rendered_match_cases

(**
 * [render_record_expr prop_lst] is the JavaScript equivalent code of
 * generating a record with the properties and values in [prop_lst]
 *
 * This is done in JavaScript by:
 * - creating an native object with its properties as the keys in [prop_lst]
 * - assigning each key to the corresponding value in [prop_lst]
 *)
and render_record_expr prop_lst =
  List.map (fun (prop, val_expr) ->
    sprintf "%s: %s" prop (render_expr val_expr)
  ) prop_lst
  |> String.concat ","
  |> sprintf "({%s})"

(**
 * [render_variant_expr name arg_expr] is the JavaScript equivalent code of
 * generating a variant with constructor name [name] and args [arg_expr].
 *
 * This is done in JavaScript by:
 * - creating an native object with its properties as "$NAME" and "$DATA".
 * - "$NAME" contains [name]
 * - "$DATA" contains the equivalent JavaScript code of [arg_expr].
 *)
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
  | Function (arg_list, body_expr) -> render_fun arg_list body_expr true
  | Sequential (expr_1, expr_2) -> render_sequential_expr expr_1 expr_2
  | LetBinding (assign, expr) -> render_let_binding_expr assign expr
  | VarName name -> Str.global_replace (Str.regexp "'") "$" name
  | FunctionCall (fn, args, curry) -> render_function_call fn args curry
  | ParenExpr (expr) -> render_paren_expr expr
  | ListExpr expr_lst -> render_list_expr expr_lst
  | ModuleAccessor (m, v) -> render_accessor m v
  | PropertyAccessor (e, v) -> render_accessor (render_expr e) v
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
  | "Array" -> Array_js.destructure
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
  "{" ^ Pervasives_js.impl ^
  List_js.impl ^
  Char_js.impl ^
  String_js.impl ^
  Array_js.impl ^
  render_module_items_list ((OpenDecl "Pervasives")::ast) ^ "}"
