(** Detects non curried functions. *)

(**
 * [optimize ast] transforms [ast] into an equivalent one with any not curryable
 * function detected.
 *
 * Every [FunctionAssignment] and [FunctionCall] will have their curry bool
 * set to false if it is determined that it doesn't need to be curried.
 *
 * Any other nodes are ignored and returned as is.
 *)
val optimize : Ast.t -> Ast.t
