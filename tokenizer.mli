(** The abstract type of values representing tokens. *)
type t

val tokenize : in_channel -> t list
