(**
 * String JS port.
 *
 * This shadows the native string and uses it as a backend for storing the
 * string.
 *)

 (**
  * Implementation of the module.
  *
  * It contains the definition of a JS object named String.
  *)
val impl : string

(**
 * Binding that brings all the module definitions into scope.
 *
 * It uses ES6 object destructuring to do this.
 *)
val destructure : string
