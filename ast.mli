type constant =
  | Int of int
  | EmptyList

type infix_op =
  | Plus | Minus | Divide | Times | GreaterThan | LessThan | GreaterThanOrEqual
  | LessThanOrEqual | Equal | NotEqual | Cons | Append

type prefix_symbol =
  | Negation

type pattern =
  | ValueName of string

type let_binding =
  | VarAssignment of pattern * expr
  | FunctionAssignment of string * bool * pattern list * expr
and expr =
  | Constant of constant
  | VarName of string
  | ModuleAccessor of string * string
  | PrefixOp of prefix_symbol * expr
  | InfixOp of expr * infix_op * expr
  | Ternary of expr * expr * expr option
  | Function of pattern list * expr
  | Sequential of expr * expr
  | LetBinding of let_binding * expr
  | FunctionCall of expr * expr
  | ParenExpr of expr
  | ListExpr of expr list
type t = expr
