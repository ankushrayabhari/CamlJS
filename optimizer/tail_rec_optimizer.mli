(** Detects tail recursive functions. **)

(**
 * [optimize ast] transforms [ast] into an equivalent one with tail recursive
 * functions identified as a [TailRecursiveFunctionAssignment].
 *
 * Only tail recursive functions that were intially let bindings not assigned
 * to anonymous functions will be converted.
 * Any other nodes are ignored and returned as is.
 *)
val optimize : Ast.t -> Ast.t
