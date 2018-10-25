type parse_tree =
  | Token of Tokenizer.token
  | Node of parse_tree list

val parse : Tokenizer.token array -> parse_tree
