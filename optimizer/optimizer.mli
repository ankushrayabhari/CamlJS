(** Transforms an AST into a more optimal AST*)

(**
 * [optimize ast] is another Abstract syntax tree equivalent to [ast] but that
 * is more optimal in the final rendered JavaScript.
 *
 * The following optimizations are run:
 * - Deduplication of identifiers: {b See:} [Deduplicate_ident]
 * - Removal of unused bindings: {b See:} [Unused_binding_optimizer]
 * - Function call squashing: {b See:} [Call_squasher]
 * - Uncurrying of functions: {b See:} [Curry_optimizer]
 * - Identify tail recursive functions: {b See:} [Tail_rec_optimizer]
 *)
val optimize : Ast.t -> Ast.t
