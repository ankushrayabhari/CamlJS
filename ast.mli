type constant =
  | Int of int
  | EmptyList
  | Bool of bool
  | StringLiteral of string
  | CharLiteral of char
  | Float of float
  | Unit

type infix_op =
  | Plus | Minus | Divide | Times | GreaterThan | LessThan | GreaterThanOrEqual
  | LessThanOrEqual | Equal | NotEqual | Cons | Append | LogicalAnd
  | LogicalOr | PlusFloat | MinusFloat | DivideFloat | TimesFloat
  | Concat

type prefix_symbol =
  | Negation | NegationFloat

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

type module_item =
  | LetDecl of let_binding
  | OpenDecl of string
  | Expr of expr

type t = module_item list
