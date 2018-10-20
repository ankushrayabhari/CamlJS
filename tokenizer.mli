(** The abstract type of values representing tokens. *)
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

val tokenize : string -> token list
