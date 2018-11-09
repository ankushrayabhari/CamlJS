(** *)

(**
 * The following optimizations are run:
 * - Deduplication of identifiers: {b See:} [Deduplicate_ident]
 * - Removal of unused bindings: {b See:} [Unused_binding_optimizer]
 * - Function call squashing: {b See:} [Call_squasher]
 * - Uncurrying of functions: {b See:} [Curry_optimizer]
 * - Identify tail recursive functions: {b See:} [Tail_rec_optimizer]
 *)
let optimize tr =
  tr
  |> Deduplicate_ident.optimize
  |> Unused_binding_optimizer.optimize
  |> Call_squasher.optimize
  |> Curry_optimizer.optimize
  |> Tail_rec_optimizer.optimize
