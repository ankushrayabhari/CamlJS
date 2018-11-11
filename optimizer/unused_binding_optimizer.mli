(** Removes any unused bindings. *)

(**
 * [optimize ast] transforms [ast] into an equivalent one with any simple unused
 * binding removed.
 *
 * A simple unused binding is:
 * - any [VarAssignment] that only contains a single [ValueNamePattern]
 * - any [FunctionAssignment] or [TailRecursiveFunctionAssignment]
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
 *        ValueNamePattern "x",
 *        true,
 *        Constant (Int 2)
 *      ),
 *      VarName "x"
 *    )
 * v}
 *
 * Aside from these pruning, no modifications are made to the tree.
 *)
val optimize : Ast.t -> Ast.t
