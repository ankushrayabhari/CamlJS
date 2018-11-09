(** Transforms an AST into a more optimal AST*)

(**
 * [optimize ast] is another Abstract syntax tree equivalent to [ast] but that
 * is more optimal in the final rendered JavaScript.
 *)
val optimize : Ast.t -> Ast.t
