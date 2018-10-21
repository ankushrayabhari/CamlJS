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

val parse : Tokenizer.token array -> expr
