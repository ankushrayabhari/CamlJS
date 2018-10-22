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
      let function_body = render_expr body_expr in
      let arguments = List.map render_pattern arg_list |> String.concat "," in
      let function_name = render_value_name name_value in
      "let " ^ function_name ^ " = (" ^ arguments ^ ") =>" ^
        function_body ^  ";"

and render_expr = function
  | Constant c -> render_constant c
  | PrefixOp (prefix, expr) -> failwith "prefix op not implemented"
  | InfixOp (l_expr, op, r_expr) ->
      let rendered_l = render_expr l_expr in
      let rendered_r = render_expr r_expr in
      let rendered_op = render_infix_op op in
      "(() => (" ^ rendered_l ^ ")" ^ rendered_op ^ "(" ^ rendered_r ^ "))()"
  | Ternary (condition, true_expr, false_expr) -> failwith "ternary not implemented"
  | Function (arg_list, body)-> failwith "functions not implemented"
  | Sequential (expr_1, expr_2) -> failwith "sequential ops not implemented"
  | LetBinding (assign, expr) ->
      let rendered_assign = render_let_binding assign in
      let rendered_in_expr = render_expr expr in
      "(() => {" ^ rendered_assign ^ "return " ^ rendered_in_expr ^ "})()"
  | VarName (val_name) -> render_value_name val_name

let render exp =
  render_expr exp
