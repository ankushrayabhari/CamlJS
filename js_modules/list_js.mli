(**
 * List JS port.
 *
 * Lists are stored as arrays natively in reverse order to add support for O(1)
 * amortized cons and O(n) appends.
 *)

(**
 * Implementation of the module.
 *)
val impl : string

(**
 * Binding that brings all the module definitions into scope.
 *)
val destructure : string
