open Parser

let render_constant = function
  | Int v -> "(() => " ^ (string_of_int v) ^ ")()"

let render_value_name = function
  | LowercaseIdent s -> s

let render_infix_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Divide -> "/"
  | Times -> "*"
  | GreaterThan -> ">"
  | LessThan -> "<"
  | GreaterThanOrEqual -> ">="
  | LessThanOrEqual -> "<="
  | Equal -> "==="
  | NotEqual -> "!=="

let render_pattern = function
  | ValueName value -> render_value_name value

let rec render_let_binding = function
  | VarAssignment (pat, expr) ->
      let var_name = render_pattern pat in
      let assignment_expr = render_expr expr in
      "let " ^ var_name ^ " = " ^ assignment_expr ^  ";"
  | FunctionAssignment (name_value, _, arg_list, body_expr) ->
      let function_name = render_value_name name_value in
      "let " ^ function_name ^ " = " ^ render_fun arg_list body_expr ^  ";"

and render_fun arg_list body_expr =
  let function_body = render_expr body_expr in
  let arguments = List.map render_pattern arg_list |> String.concat "," in
  "((" ^ arguments ^ ") =>" ^ function_body ^ ")"

and render_prefix_op = function
  | Negation -> "-"

and render_ternary condition_expr true_expr false_expr =
  let rendered_condition_expr = render_expr condition_expr in
  let rendered_true_expr = render_expr true_expr in
  let rendered_false_expr = match false_expr with
    | None -> "(() => {})()"
    | Some expr -> render_expr expr
  in
  "(() => " ^
    rendered_condition_expr ^ "? " ^
    rendered_true_expr ^ " : " ^
    rendered_false_expr ^
  ")()"

and render_prefix_expr prefix expr =
  let rendered_op = render_prefix_op prefix in
  let rendered_expr = render_expr expr in
  "(() => " ^ rendered_op ^ "(" ^ rendered_expr ^ "))()"

and render_infix_expr l_expr op r_expr =
  let rendered_l = render_expr l_expr in
  let rendered_r = render_expr r_expr in
  let rendered_op = render_infix_op op in
  "(() => (" ^ rendered_l ^ ")" ^ rendered_op ^ "(" ^ rendered_r ^ "))()"

and render_sequential_expr expr_1 expr_2 =
  let rendered_1 = render_expr expr_1 in
  let rendered_2 = render_expr expr_2 in
  "(() => {" ^ rendered_1 ^ "; return " ^ rendered_2 ^ "})()"

and render_let_binding_expr assign expr =
  let rendered_assign = render_let_binding assign in
  let rendered_in_expr = render_expr expr in
  "(() => {" ^ rendered_assign ^ "return " ^ rendered_in_expr ^ "})()"

and render_expr = function
  | Constant c -> render_constant c
  | PrefixOp (prefix, expr) -> render_prefix_expr prefix expr
  | InfixOp (l, op, r) -> render_infix_expr l op r
  | Ternary (c, t, f) -> render_ternary c t f
  | Function (arg_list, body_expr) -> render_fun arg_list body_expr
  | SequentialWithValue (expr_1, expr_2) -> render_sequential_expr expr_1 expr_2
  | LetBinding (assign, expr) -> render_let_binding_expr assign expr
  | VarName (val_name) -> render_value_name val_name
  | Sequential (expr_1, expr_2) -> failwith "sequential overall not implemented"

let render exp =
  render_expr exp
