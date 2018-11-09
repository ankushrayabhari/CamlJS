(** Parse Tree Nodes *)

(**
 * A [parse_tree] is a representation of the program as a tree of its
 * components, where the leaves in the tree read from left to right are the
 * tokens of the program in order from left to right:
 * - [Token tok] is a token where [tok] is a [Token.t].
 * - [Node lst] is a non-leaf node in the tree where [lst] is a
 * [parse_tree list] of its children nodes.
 *
 * Note that all unit production paths in the parse tree get squashed to a
 * single edge.
 *)
type t =
  | Token of Token.t
  | Node of t list
