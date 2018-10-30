open Ast

let render_constant = function
  | Int v -> string_of_int v
  | EmptyList -> "[]"
  | Bool b -> string_of_bool b
  | Float f -> string_of_float f
  | StringLiteral s -> s
  | CharLiteral c -> String.make 1 c
  | Unit -> "null"

let render_pattern = function
  | ValueNamePattern name -> name

let rec render_let_binding = function
  | VarAssignment (pat, expr) ->
      let var_name = render_pattern pat in
      let assignment_expr = render_expr expr in
      "let " ^ var_name ^ " = " ^ assignment_expr ^  ";"
  | FunctionAssignment (function_name, _, arg_list, body_expr) ->
      "let " ^ function_name ^ " = " ^ render_fun arg_list body_expr ^  ";"

and render_fun arg_list body_expr =
  let function_body = render_expr body_expr in
  let arguments =
    List.map render_pattern arg_list |>
    List.fold_left (fun acc el ->
      acc ^ "(" ^ el ^ ") => "
    ) ""
  in
  "(" ^ arguments ^ function_body ^ ")"

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

and render_expr = function
  | Constant c -> render_constant c
  | PrefixOp (prefix, expr) -> render_prefix_expr prefix expr
  | InfixOp (l, op, r) -> render_infix_expr l op r
  | Ternary (c, t, f) -> render_ternary c t f
  | Function (arg_list, body_expr) -> render_fun arg_list body_expr
  | Sequential (expr_1, expr_2) -> render_sequential_expr expr_1 expr_2
  | LetBinding (assign, expr) -> render_let_binding_expr assign expr
  | VarName name -> name
  | FunctionCall (expr_1, expr_2) -> render_function_call expr_1 expr_2
  | ParenExpr (expr) -> render_paren_expr expr
  | ListExpr expr_lst -> render_list_expr expr_lst
  | ModuleAccessor (m, v) -> render_module_accessor m v

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
