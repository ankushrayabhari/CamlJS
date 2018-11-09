(** Squashes function calls into one node. *)

(**
 * [optimize ast] transforms [ast] into an equivalent one with function calls
 * squashed into a single AST node.
 *
 * {b Example:}
 * - From the immediate AST conversion on the parse tree a function call [f 1 2]
 * will be transformed into:
 * {v
 *    FunctionCall (
 *      FunctionCall (
 *        VarName "f",
 *        [Constant (Int 1)],
 *        true
 *      ),
 *      [Constant (Int 2)],
 *      true
 *    )
 * v}
 * - However, because the function [f] is semantically applied to both [1] and
 * [2], this will be transformed into:
 * {v
 *    FunctionCall (
 *      VarName "f",
 *      [Constant (Int 1); Constant (Int 2)],
 *      true
 *    )
 * v}
 *
 * Any non-function call nodes are ignored and returned as is.
 *)
val optimize : Ast.t -> Ast.t
