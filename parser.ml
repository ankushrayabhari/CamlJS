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

let token_to_varid = Tokenizer.(function
  | Int _ -> [1; 40]
  | Plus -> [24]
  | Minus -> [2]
  | Times -> [3]
  | Divide -> [4]
  | GreaterThan -> [5]
  | LessThan -> [6]
  | GreaterThanOrEqual -> [7]
  | LessThanOrEqual -> [8]
  | NotEqual -> [9]
  | Equal -> [10]
  | Negation -> [11]
  | LowercaseIdent _ -> [12; 33; 41; 40]
  | FunctionArrow -> [13]
  | LParen -> [14]
  | RParen -> [15]
  | If -> [16]
  | Then -> [17]
  | Else -> [18]
  | Fun -> [19]
  | SemiColon -> [20]
  | Let -> [21]
  | Rec -> [22]
  | In -> [23]
)

let rules = [
  [(40, 15)]; (* 25 *)
  [(40, 40); (40, 26)]; (* 26 *)
  [(24, 40); (2, 40); (3, 40); (4, 40); (5, 40); (6, 40); (7, 40); (8, 40); (9, 40); (10, 40);]; (* 27 *)
  [(40, 29)]; (* 28 *)
  [(17, 40); (17, 30)]; (* 29 *)
  [(40, 31)]; (* 30 *)
  [(18, 40)]; (* 31 *)
  [(20, 40)]; (* 32 *)
  [(33, 33)]; (* 33 *)
  [(41, 35)]; (* 34 *)
  [(13, 40)]; (* 35 *)
  [(10, 40)]; (* 36 *)
  [(12, 36); (12, 39)]; (* 37 *)
  [(22, 37)]; (* 38 *)
  [(33, 36)]; (* 39 *)
  [(14, 25); (11, 40); (40, 27); (16, 29); (19, 34); (40, 32); (21, 38); (21, 37); (40, 40); (40, 26)]; (* 40 *)
  [(41, 41)]; (* 41 *)
]

let parse tok_arr =
  let n = Array.length tok_arr in
  let dp = Array.init n (fun _ -> Array.init n (fun _ -> (Array.make 42 false))) in
  for s = 0 to n - 1 do
    List.iter (fun v ->
      dp.(s).(s).(v) <- true
    ) (token_to_varid tok_arr.(s))
  done;

  for l = 1 to n - 1 do
    for s = 0 to n - 1 - l do
      for m = s to n - 1 do
        List.iteri (fun a el ->
          let a = a + 25 in
          List.iter (fun (b, c) ->
            if dp.(s).(m).(b) && dp.(m).(s + l).(c)
            then dp.(s).(s + l).(a) <- true
          ) el
        ) rules
      done
    done
  done;
  if not dp.(n - 1).(n - 1).(40) then failwith "asdf"
  else failwith "yayyyyy"
