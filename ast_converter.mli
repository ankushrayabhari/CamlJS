(**
* [convert tr] is the representation of [tr] in ast form. 
* Requires:
* - [tr] is a valid supported parse tree
*)
val convert : Parser.parse_tree -> Ast.t
