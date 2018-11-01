(**
 * Pervasives JS port.
 *
 * Each method is stored as part of a global Pervasives object. Native strings
 * are aliased as NativeString.
 *)

(**
 * Implementation of the module.
 *)
val impl : string

(**
 * Binding that brings all the module definitions into scope.
 *)
val destructure : string
