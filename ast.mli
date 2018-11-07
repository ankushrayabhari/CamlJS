(** Abstract Syntax Tree Nodes *)

type constant =
  | Int of int
  | EmptyList
  | EmptyArray
  | Bool of bool
  | StringLiteral of string
  | CharLiteral of string
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
  | IgnorePattern
  | ValueNamePattern of string
  | ConstantPattern of constant
  | AliasPattern of pattern * string
  | ParenPattern of pattern
  | ListPattern of pattern list
  | ArrayPattern of pattern list
  | ConsPattern of pattern * pattern
  | RangedCharacterPattern of string * string
  | VariantPattern of string * pattern option
  | TuplePattern of pattern list
  | RecordPattern of (string * pattern) list

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
  | ArrayExpr of expr list
  | MatchExpr of expr * (pattern * expr * expr option) list
  | Tuple of expr list
  | Record of (string * expr) list
  | Variant of string * expr option

type tuple =
  | String of string
  | Tuple of tuple list

type type_definition =
  | VariantDecl of string * (string * tuple list) list
  | RecordDecl of (string * string) list

type module_item =
  | LetDecl of let_binding
  | OpenDecl of string
  | Expr of expr
  | TypeDefinition of type_definition

type t = module_item list
