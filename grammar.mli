val token_to_varid : Token.t -> int list

val rules : (int * int) list list

val start_variable : int

val num_tokens : int

val num_variables : int

val auto_generated_variable : int -> bool
