(** Converter from Parse Tree to AST *)

(**
 * [convert parse_tr] is the ast conversion of the declarations and expressions
 * of the module parse tree [tr].
 *
 * @raise Failure if [tr] is not valid module items parse tree.
*)
val convert : Parser.parse_tree -> Ast.t
