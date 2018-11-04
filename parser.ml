type parse_tree =
  | Token of Token.t
  | Node of parse_tree list

let parse =
  failwith "unimplemented"
