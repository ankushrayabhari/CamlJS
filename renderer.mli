(** Converts the AST to JS Code *)

(**
* [render tr] is the JavaScript code equivalent to the code represented by
* abstract syntax tree [tr].
*)
val render : Ast.t -> string
