type parse_tree =
  | Token of Token.t
  | Node of parse_tree list

val parse : Token.t array -> parse_tree
