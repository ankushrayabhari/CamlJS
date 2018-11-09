(** Shift Reduce Parse Actions *)

(**
 * A [lr_action] is a possible state transition that a shift reduce parser can
 * take in a given state.
 * - [Shift s] means parse a token from the input onto the stack and move to
 * state [s].
 * - [Reduce prod_size var_id] means pop a production of length [prod_size]
 * from the stack and combine it into [var_id].
 * - [Accept] means the current stack state contains a valid parse tree.
 * - [Goto s] means move to state [s].
 * - [Error] means the current string is invalid.
 *)
type t =
 | Shift of int
 | Reduce of int * int
 | Accept
 | Goto of int
 | Error
