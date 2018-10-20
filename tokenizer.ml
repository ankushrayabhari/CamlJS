type token =
  | Int of int
  | PlusOperator
  | MinusOperator
  | TimesOperator
  | DivideOperator
  | GreaterThanOperator
  | LessThanOperator
  | GreaterThanOrEqualOperator
  | LessThanOrEqualOperator
  | NotEqualOperator
  | EqualOperator
  | NegationOperator
  | LowercaseIdent of string
  | FunctionArrow
  | LParen
  | RParen
  | IfKeyword
  | ThenKeyword
  | ElseKeyword
  | FunctionKeyword
  | SemiColon
  | LetKeyword
  | RecKeyword
  | InKeyword
type t = token

let tokenize str =
  failwith "unimplemented"
