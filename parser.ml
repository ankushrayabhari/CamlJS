type constant =
  | Int of int

type infix_op =
  | Plus | Minus | Divide | Times | GreaterThan | LessThan | GreaterThanOrEqual
  | LessThanOrEqual | Equal | NotEqual

type prefix_symbol =
  | Negation

type value_name =
  | LowercaseIdent of string

type pattern =
  | ValueName of value_name

type let_binding =
  | VarAssignment of pattern * expr
  | FunctionAssignment of value_name * bool * pattern list * expr
and expr =
  | Constant of constant
  | PrefixOp of prefix_symbol * expr
  | InfixOp of expr * infix_op * expr
  | Ternary of expr * expr * expr option
  | Function of pattern list * expr
  | Sequential of expr * expr
  | LetBinding of let_binding * expr
  | VarName of value_name
  | FunctionCall of expr * expr
type ast = expr

let token_to_varid = Tokenizer.(function
    | Int _ -> [1; 25]
    | Plus -> [2]
    | Minus -> [3]
    | Times -> [4]
    | Divide -> [5]
    | GreaterThan -> [6]
    | LessThan -> [7]
    | GreaterThanOrEqual -> [8]
    | LessThanOrEqual -> [9]
    | NotEqual -> [10]
    | Equal -> [11]
    | Negation -> [12]
    | LowercaseIdent _ -> [13;25]
    | FunctionArrow -> [14]
    | LParen -> [15]
    | RParen -> [16]
    | If -> [17]
    | Then -> [18]
    | Else -> [19]
    | Fun -> [20]
    | SemiColon -> [21]
    | Let -> [22]
    | Rec -> [23]
    | In -> [24]
  )

let rules = [
  [(15, 26); (12, 25); (25, 27); (17, 28); (20, 32); (25, 35); (22, 36);
   (22, 37); (25, 25)]; (* 25 *)
  [(25, 16)]; (* 26 *)
  [(2, 25); (3, 25); (4, 25); (5, 25); (6, 25); (7, 25); (8, 25); (9, 25);
   (10, 25); (11, 25)]; (* 27 *)
  [(25, 29)]; (* 28 *)
  [(18, 25); (18, 30)]; (* 29 *)
  [(25, 31)]; (* 30 *)
  [(19, 25)]; (* 31 *)
  [(33, 34)]; (* 32 *)
  [(13, 33)]; (* 33 *)
  [(14, 25)]; (* 34 *)
  [(21, 25)]; (* 35 *)
  [(23, 37)]; (* 36 *)
  [(38, 40)]; (* 37 *)
  [(13, 39); (33, 39)]; (* 38 *)
  [(11, 25)]; (* 39 *)
  [(24, 25)]; (* 40 *)
]

type parse_tree =
  | Cons of int * int * int * parse_tree * parse_tree
  | Nil

let production_of_root_of_parse_tree = function
  | Cons (_, _, _, Cons (l, _, _, _, _), Cons (r, _, _, _, _)) -> Some (l, r)
  | _ -> None

let parse_infix_op tok_arr = function
  | Nil -> failwith "should not be called on nil"
  | Cons (_, s, _, _, _) ->
      begin match tok_arr.(s) with
        | Tokenizer.Plus -> Plus
        | Tokenizer.Minus -> Minus
        | Tokenizer.Times -> Times
        | Tokenizer.Divide -> Divide
        | Tokenizer.GreaterThan -> GreaterThan
        | Tokenizer.LessThan -> LessThan
        | Tokenizer.GreaterThanOrEqual -> GreaterThanOrEqual
        | Tokenizer.LessThanOrEqual -> LessThanOrEqual
        | Tokenizer.NotEqual -> NotEqual
        | Tokenizer.Equal -> Equal
        | _ -> failwith "infix operation not supported"
      end

let parse_prefix_op tok_arr = function
  | Nil -> failwith "should not be called on nil"
  | Cons (_, s, _, _, _) ->
    begin match tok_arr.(s) with
      | Tokenizer.Negation -> Negation
      | _ -> failwith "prefix operation not supported"
    end

let parse_token_expr tok_arr = function
  | Nil -> failwith "should not be called on nil"
  | Cons (_, s, _, _, _) ->
      begin match tok_arr.(s) with
        | Tokenizer.Int v -> Constant (Int v)
        | Tokenizer.LowercaseIdent v -> VarName (LowercaseIdent v)
        | _ -> failwith "unsupported token expr value"
      end

let rec get_params tok_arr s e (params:pattern list) =
  let next_param = match tok_arr.(s) with
    | Tokenizer.LowercaseIdent n -> ValueName (LowercaseIdent n)
    | _ -> failwith "not a valid function parameter" in
  if s=e then List.rev(next_param::params)
  else get_params tok_arr (s+1) e (next_param::params)

let rec parse_infix_expr tok_arr = function
  | Nil -> failwith "should not be called on nil"
  | Cons (_, _, _, l, Cons (_, _, _, lr, rr)) ->
      let left_operand = parse_expr tok_arr l in
      let operator = parse_infix_op tok_arr lr in
      let right_operand = parse_expr tok_arr rr in
      InfixOp (left_operand, operator, right_operand)
  | _ -> failwith "not an infix expr"

and parse_prefix_expr tok_arr = function
  | Nil -> failwith "should not be called on nil"
  | Cons (_, _, _, l, r) ->
    let right_operand = parse_expr tok_arr r in
    let operator = parse_prefix_op tok_arr l in
    PrefixOp (operator, right_operand)

and parse_paren_expr tok_arr = function
  | Nil -> failwith "should not be called on nil"
  | Cons (_, _, _, _, Cons (_, _, _, lr, _)) ->
      parse_expr tok_arr lr
  | _ -> failwith "not a parenthesized expr"

and parse_fun_expr tok_arr = function
  | Nil -> failwith "should not be called on nil"
  | Cons (_, _, _, _,
          Cons (_, _, _, Cons (_, s, e, _, _), Cons (_, _, _, _, ex))) ->
    Function (get_params tok_arr s e [], (parse_expr tok_arr ex))
  | _ -> failwith "not a function expr"

and parse_function_call_expr tok_arr = function
  | Nil   -> failwith "should not be called on nil"
  | Cons (_, _, _, l, r) -> begin
      let f_name = parse_expr tok_arr l in
      let f_args = parse_expr tok_arr r in
      FunctionCall(f_name, f_args)
    end

and parse_let_binding_expr tok_arr = function
  | Nil -> failwith "should not be called on nil"
  | Cons (_, _, _, _,
      Cons (_, _, _,
        Cons (_, _, _,
          Cons (_, ident_token_index, _, _, _),
          Cons (_, _, _, _, assignment_expr_var_tree)
        ),
        Cons (_, _, _, _, in_expr_var_tree)
      )
    ) ->
      begin
        match tok_arr.(ident_token_index) with
              | Tokenizer.LowercaseIdent s ->
                  LetBinding (
                    VarAssignment (
                      ValueName (LowercaseIdent s),
                      parse_expr tok_arr assignment_expr_var_tree
                    ),
                    parse_expr tok_arr in_expr_var_tree
                  )
              | _ -> failwith "invalid let assignment tree"
      end
    (* implement function let assign *)
    | _ -> failwith "not a let assign expr"

and parse_rec_expr tok_arr = function
  | Nil -> failwith "should not be called on nil"
  | Cons (_, _, _, _,
          Cons (_, _, _, _,
                Cons (_, _, _,
                      Cons (_, _, _,
                            Cons (_, s, e, _, _),
                            Cons (_, _, _, _, equals_expr)
                           ),
                      Cons (_, _, _, _, in_expr)
                     )
               )
         ) ->
    let first = begin match tok_arr.(s) with
      | Tokenizer.LowercaseIdent n -> LowercaseIdent n
      | _ -> failwith "invalid function name"
    end in
    let params = if s=e then [] else get_params tok_arr (s+1) e [] in
    LetBinding (FunctionAssignment (first, true, params,
                                    parse_expr tok_arr equals_expr),
                parse_expr tok_arr in_expr)
  | _ -> failwith "not a rec expr"


and parse_semicolon_expr tok_arr = function
  | Nil -> failwith "should not be called on nil"
  | Cons (_, _, _, pre_semicolon_tree, Cons (_, _, _, _, post_semicolon_tree)) ->
    let pre_semicolon_expr = parse_expr tok_arr pre_semicolon_tree in
    let post_semicolon_expr = parse_expr tok_arr post_semicolon_tree in
    Sequential (pre_semicolon_expr, post_semicolon_expr)
  | _ -> failwith "not a semicolon expression"

and parse_if_no_else_body tok_arr cond_expr = function
  | Cons (_,_,_,_,then_body_tree) ->
    Ternary (cond_expr, (parse_expr tok_arr then_body_tree), None)
  | Nil -> failwith "if without else has no body (shouldn't be called)"

and parse_if_else_body tok_arr cond_expr = function
  | Cons (_, _, _, _,
      Cons (30, _, _,
        Cons (25, _, _, _, on_if_true_tree),
        Cons (31, _, _, _, on_if_false_tree)
      )
    ) ->
    Ternary (
      cond_expr,
      parse_expr tok_arr on_if_true_tree,
      Some (parse_expr tok_arr on_if_false_tree)
    )
  | Nil -> failwith "if with else somehow failed"
  | _ -> failwith "should not be called"

and parse_if_expr tok_arr = function
  | Nil -> failwith "should not be called on nil"
  | Cons (_,_,_,_,Cons (_,_,_,cond_tree,if_body_tree)) ->
    let cond_expr = parse_expr tok_arr cond_tree in
    begin match production_of_root_of_parse_tree cond_tree with
      | Some (18,25) -> parse_if_no_else_body tok_arr cond_expr if_body_tree
      | Some (18,30) -> parse_if_else_body tok_arr cond_expr if_body_tree
      | _-> failwith "invalid if statement production"
    end
  | _ -> failwith "invalid if statement parse tree"

and parse_expr tok_arr = function
  | Nil -> failwith "should not be called on nil"
  | Cons (v, s, e, l, r) as t ->
      match production_of_root_of_parse_tree t with
        | None -> parse_token_expr tok_arr t
        | Some (15, 26) -> parse_paren_expr tok_arr t
        | Some (12, 25) -> parse_prefix_expr tok_arr t
        | Some (25, 27) -> parse_infix_expr tok_arr t
        | Some (17, 28) -> parse_if_expr tok_arr t
        | Some (20, 32) -> parse_fun_expr tok_arr t
        | Some (25, 35) -> parse_semicolon_expr tok_arr t
        | Some (22, 36) -> parse_rec_expr tok_arr t
        | Some (22, 37) -> parse_let_binding_expr tok_arr t
        | Some (25, 25) -> parse_function_call_expr tok_arr t
        | _ -> failwith "invalid production rule"

let iterate_over_productions f =
  List.iteri (fun a el ->
    let a = a + 25 in
    List.iter (fun (b, c) ->
        f (a,b,c)
    ) el
  ) rules

let parse tok_arr =
  let n = Array.length tok_arr in
  let dp = Array.init n
    (fun _ -> Array.init n (fun _ -> (Array.make 42 false))) in
  let prev = Array.init n
    (fun _ -> Array.init n (fun _ -> (Array.make 42 (-1, -1, -1)))) in
  for s = 0 to n - 1 do
    List.iter (fun v ->
        dp.(s).(s).(v) <- true;
        prev.(s).(s).(v) <- (-1, -1, -1)
      ) (token_to_varid tok_arr.(s))
  done;

  for l = 1 to n - 1 do
    for s = 0 to n - 1 - l do
        for m = s to s + l - 1 do
          iterate_over_productions (fun (a, b, c) ->
            if not dp.(s).(s + l).(a) &&
                   dp.(s).(m).(b) &&
                  dp.(m + 1).(s + l).(c)
            then begin
              dp.(s).(s + l).(a) <- true;
              prev.(s).(s+l).(a) <- (b, m, c);
            end
          )
        done
    done
  done;

  if not dp.(0).(n - 1).(25) then failwith "Invalid program."
  else
    let rec generate_var_tree s e v =
      if (s = e) then Cons (v, s, e, Nil, Nil) else
      let (left_var, middle_index, right_var) = prev.(s).(e).(v) in
      let left_parse_tree = generate_var_tree s middle_index left_var in
      let right_parse_tree = generate_var_tree (middle_index + 1) e right_var in
      Cons (v, s, e, left_parse_tree, right_parse_tree)
    in
    let expr_var_tree = generate_var_tree 0 (n - 1) 25 in
    parse_expr tok_arr expr_var_tree
