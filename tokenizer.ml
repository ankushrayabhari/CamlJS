open Str

type token =
  (* Constants *)
  | Int of int
  (* Identifiers *)
  | LowercaseIdent of string
  (* Arithmetic Operators *)
  | Plus | Minus | Times | Divide | Negation
  (* Comparison Operators *)
  | GreaterThan | LessThan | GreaterThanOrEqual | LessThanOrEqual | NotEqual
  | Equal
  (* Special character sequences. *)
  | FunctionArrow | LParen | RParen | SemiColon
  (* Keywords *)
  | If | Then | Else | Function | Let | Rec | In

let regexp_of_token tok = regexp (match tok with
  | Int _ ->  "[0-9]+"
  | Plus -> "\\+"
  | Minus -> "-"
  | Times -> "\\*"
  | Divide -> "/"
  | GreaterThan -> ">"
  | LessThan -> "<"
  | GreaterThanOrEqual -> ">="
  | LessThanOrEqual -> "<="
  | NotEqual -> "<>"
  | Equal -> "="
  | Negation -> "~-"
  | LowercaseIdent _ -> "\\([a-z]\\|_\\)\\([A-Za-z0-9]\\|_\\|'\\)*"
  | FunctionArrow -> "->"
  | LParen -> "("
  | RParen -> ")"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | Function -> "fun"
  | SemiColon -> ";"
  | Let -> "let"
  | Rec -> "rec"
  | In -> "in"
)

let precedence = [
  (* Two letter operators. *)
  NotEqual; GreaterThanOrEqual; LessThanOrEqual; FunctionArrow; Negation;

  (* Single letter operators. *)
  Plus; Minus; Times; Divide; GreaterThan; LessThan; Equal; LParen; RParen;
  SemiColon;

  (* Keywords. *)
  If; Then; Else; Function; Let; Rec; In;

  (* Constants *)
  Int 0;

  (* Identifiers *)
  LowercaseIdent "";
]

let token_of_string str tok = match tok with
  | Int _ -> Int (int_of_string str)
  | LowercaseIdent _ -> LowercaseIdent str
  | t -> t

let rec tokenize_rec str start tok_lst =
  let whitespace_regex = Str.regexp "[ \n\r\t]*" in
  let _ = Str.string_match whitespace_regex str start in
  let start_index = start + (String.length (Str.matched_string str)) in
  if start_index >= (String.length str) then tok_lst
  else
    let token = List.fold_left (fun acc curr_tok -> match acc with
        | (Some _, _) -> acc
        | (None, _) ->
          let regex = regexp_of_token curr_tok in
          if Str.string_match regex str start_index
          then
            let matched_str = Str.matched_string str in
            let len = String.length matched_str in
            (Some (token_of_string matched_str curr_tok), len)
          else (None, -1)
      ) (None, -1) precedence
    in
    match token with
    | (Some tok, len) -> tokenize_rec str (start_index + len) (tok::tok_lst)
    | (None, _) ->
        failwith ("Unknown symbol at " ^ string_of_int start_index ^ ".")

let tokenize str =
  tokenize_rec str 0 [] |> List.rev
