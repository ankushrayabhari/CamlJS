(**
 * Char JS port.
 *
 * Single character strings are used as the storage backend for characters.
 *)

 (**
  * Implementation of the module.
  *
  * It contains the definition of a JS object named Char.
  *)
val impl : string

(**
 * Binding that brings all the module definitions into scope.
 *
 * It uses ES6 object destructuring to do this.
 *)
val destructure : string
