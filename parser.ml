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
  [(25, 16)]; (* 26 *)
  [(2, 25); (3, 25); (4, 25); (5, 25); (6, 25); (7, 25); (8, 25); (9, 25); (10, 25); (11, 25)]; (* 27 *)
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
  if not dp.(n - 1).(n - 1).(25) then failwith "asdf"
  else failwith "yayyyyy"
