let optimize tr =
  tr
  |> Call_squasher.optimize
  (* |> Curry_optimizer.optimize *)
  (* |> Unused_binding_optimizer.optimize *)
  |> Tail_rec_optimizer.optimize
