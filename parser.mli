(** 
 * A [parse_tree] is a representation of the program as a tree of its
 * components, where the leaves in the tree read from left to right are the
 * tokens of the program in order from left to right:
 * - [Token tok] is a token where [tok] is a [Token.t].
 * - [Node lst] is a non-leaf node in the tree where [lst] is a
 * [parse_tree list] of its children nodes.
 *)
type parse_tree =
  | Token of Token.t
  | Node of parse_tree list

(** 
 * A [parse arr] is a [parse_tree tr] representing the token array [arr].
 *)
val parse : Token.t array -> parse_tree
