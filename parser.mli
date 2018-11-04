(** Converts from a Token array to a Parse Tree *)


(**
 * A [parse arr] is a [parse_tree tr] representing the token array [arr].
 *)
val parse : Token.t array -> Parse_tree.t
