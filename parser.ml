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
  [(15, 26); (12, 25); (25, 27); (17, 28); (20, 32); (25, 35); (22, 36); (22, 37); (25, 25)]; (* 25 *)
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
      for m = s to s + l - 1 do
        List.iteri (fun a el ->
            let a = a + 25 in
            List.iter (fun (b, c) ->
                if not dp.(s).(s + l).(a) && dp.(s).(m).(b) && dp.(m + 1).(s + l).(c)
                then dp.(s).(s + l).(a) <- true;
              ) el
          ) rules
      done
    done
  done;

  for i = 0 to n - 1 do
    print_endline (string_of_int i ^ ": ");
    for j = 0 to n - 1 do
      print_string ("\t" ^ string_of_int j ^ ": ");
      for k = 1 to 41 do
        if dp.(i).(j).(k) then print_string (string_of_int k ^ "; ")
      done;
      print_endline ""
    done
  done;
  if not dp.(n - 1).(n - 1).(40) then failwith "asdf"
  else failwith "yayyyyy"
