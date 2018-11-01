(**
 * String JS port.
 *
 * This shadows the native string and uses it as a backend for storing the
 * string.
 *)

(**
 * Implementation of the module.
 *)
val impl : string

(**
 * Binding that brings all the module definitions into scope.
 *)
val destructure : string
