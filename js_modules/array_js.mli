(**
 * Array JS port.
 *
 * Arrays are stored as JS arrays. Native arrays are shadowed and aliased by
 * NativeArray.
 *)

 (**
  * Implementation of the module.
  *
  * It contains the definition of a JS object named Array.
  *)
val impl : string

(**
 * Binding that brings all the module definitions into scope.
 *
 * It uses ES6 object destructuring to do this.
 *)
val destructure : string
