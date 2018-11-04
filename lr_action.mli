type t =
 | Shift of int
 | Reduce of int
 | Accept
 | Goto of int
 | Error
