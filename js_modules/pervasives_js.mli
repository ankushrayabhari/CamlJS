(**
 * Pervasives JS port.
 *
 * Each method is stored as part of a global Pervasives object. Native strings
 * are aliased as NativeString.
 *)

(**
 * Implementation of the module.
 *
 * It contains the definition of a JS object named Pervasives.
 *)
val impl : string

(**
 * Binding that brings all the module definitions into scope.
 *
 * It uses ES6 object destructuring to do this.
 *)
val destructure : string
