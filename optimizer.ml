let optimize tr =
  tr
  |> Deduplicate_ident.optimize
  |> Unused_binding_optimizer.optimize
  |> Call_squasher.optimize
  |> Curry_optimizer.optimize
  |> Tail_rec_optimizer.optimize
