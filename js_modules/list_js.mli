(**
 * List JS port.
 *
 * Lists are stored as arrays natively in reverse order to add support for O(1)
 * amortized cons and O(n) appends.
 *)

 (**
  * Implementation of the module.
  *
  * It contains the definition of a JS object named List.
  *)
val impl : string

(**
 * Binding that brings all the module definitions into scope.
 *
 * It uses ES6 object destructuring to do this.
 *)
val destructure : string
