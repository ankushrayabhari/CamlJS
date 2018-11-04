type t =
 | Shift of int
 | Reduce of int * int
 | Accept
 | Goto of int
 | Error
