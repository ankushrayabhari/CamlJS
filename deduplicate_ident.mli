(** Deduplicates any identifiers. *)

(**
 * [optimize ast] transforms [ast] into an equivalent one with any lowercase
 * ident deduplicated and assigned to a unique name.
 *
 * Record properties, variant names, module names and functions in built in
 * modules are not renamed.
 *
 * {b Example:}
 * - From the immediate AST conversion on the parse tree a function call
 * [let x = 1 in let x = 2 in x] will be transformed into:
 * {v
 *    LetBinding (
 *      VarAssignment (
 *        ValueNamePattern "x",
 *        true,
 *        Constant (Int 1)
 *      ),
 *      LetBinding (
 *        VarAssignment (
 *          ValueNamePattern "x",
 *          true,
 *          Constant (Int 2)
 *        ),
 *        VarName "x"
 *      )
 *    )
 * v}
 * - The transformed ast will deduplicate [x] and assign each binding a
 * unique identifier:
 * {v
 *    LetBinding (
 *      VarAssignment (
 *        ValueNamePattern "a",
 *        true,
 *        Constant (Int 1)
 *      ),
 *      LetBinding (
 *        VarAssignment (
 *          ValueNamePattern "b",
 *          true,
 *          Constant (Int 2)
 *        ),
 *        VarName "b"
 *      )
 *    )
 * v}
 *
 * Aside from renaming identifiers, no modifications are made to the tree.
 *)
val optimize : Ast.t -> Ast.t
