(** Autogeneration script of tokenizer/grammar metadata from an grammar file.
 *
 * The grammar json file should be an association with a [tokens] and
 * a [productions] member, where each [tok] in [tokens] is a
 * token recognizable by the grammar, and each [prod] in [productions] is
 * a production rule using the tokens in [tokens].
 *
 * Each [tok] is represented by an assoc with the following structure:
 * - a [name] attribute which is the name of [tok]
 * - a [regex] attribute which is the regular expression representing [tok]
 * - a [tag] attribute which is what kind of [tok] it is
 *
 * Each [prod] is an assoc key-value pair [p, rules] where:
 * - [p] is a production
 * - [rules] is a list of production rules containing [p] where each item is a
 * list [t1..tn] representing the production rule t1 -> t2 t3 ... tn
 *
 * Note that you cannot have unit production cycles or epsilon productions in
 * your grammar.
 *
 * Please see grammar.json for an example.
 *)

open Yojson.Basic
open Yojson.Basic.Util
open Printf

(**
 * [token] contains the information represented by a token in the grammar.
 * - [name] represents the name of the token that is used as an idenfitier in
 * the grammar file.
 * - [regex] is the OCaml regular expression that matches against valid strings
 * that represent this token.
 * - [tag] is an optional parameter that denotes whether or not the token had a
 * specific tag. Currently, the two tags used are prefix and infix.
 * - [parameter] is an optional parameter that contains any data associated
 * with the token. Current types supported are [int], [string], [float], [bool],
 * and [char].
 *)
type token = {
  name: string;
  regex: string;
  tag: string option;
  parameter: string option;
}

(**
 * [variable] contains the information represented by a variable in the grammar.
 * - [name] represents the name of the variable that is used as an idenfitier in
 * the grammar file.
 * - [productions] is a list of the production rules that appear on the right
 * hand side of [variable] in the grammar.
 * - Each production is a list of token and variable identifiers that
 * represents the concatenation of the elements from left to right.
 *)
type variable = {
  name: string;
  productions: string array array;
}

module IntSet = Set.Make (
  struct
    let compare = Pervasives.compare
    type t = int
  end
)

(**
 * [parse_token json] is the token record that contains the information in
 * [json].
 *
 * {b Requires:}
 * - [json] must be a [`Assoc].
 * - [json] must have a member [name] of type [`String]
 * - [json] must have a member [regex] of type [`String]
 * - [json] can have a member [tag] or [parameter], both of type [`String]
 *
 * @raise Type_error if [json] does not conform to the above structure.
 *)
let parse_token json =
  {
    name = member "name" json |> to_string;
    regex = member "regex" json |> to_string;
    tag = member "tag" json  |> to_string_option;
    parameter = member "parameter" json  |> to_string_option;
  }

(**
 * [parse_variable (name, productions)] is the variable record that contains the
 * information in the tuple.
 * - [name] is the idenfitier of the variable.
 * - [productions] is a JSON of type [`List] where each element is a
 * [`String `List]
 * @raise Type_error if [json] does not conform to the above structure.
 *)
let parse_variable (n, p) = {
  name = n;
  productions =
    p
    |> to_list
    |> Array.of_list
    |> Array.map (fun el ->
        el
        |> to_list
        |> Array.of_list
        |> Array.map to_string
      )
}

(**
 * [get_tokens json] is the list of tokens contained in the "tokens" property of
 * [json].
 * @raise Type_error if [json] does not have a list "tokens" member or if each
 * token does not match the signature of a token.
 *)
let get_tokens grammar_json =
  member "tokens" grammar_json
  |> to_list
  |> Array.of_list
  |> Array.map parse_token
  |> Array.append [|{
      name = "_EOF";
      regex = "";
      tag = None;
      parameter = None;
    }|]

(**
 * [get_variables json] is the list of variables contained in the "productions"
 * property of [json].
 * @raise Type_error if [json] does not have a list "productions" member or if
 * each variable does not match the signature of a variable.
 *)
let get_variables grammar_json =
  member "productions" grammar_json
  |> to_assoc
  |> List.map parse_variable
  |> Array.of_list
  |> (fun variables ->
      let hd = Array.get variables 0 in
      Array.append [|{
        name = "_START";
        productions = [|[|
          hd.name; "_EOF"
        |]|];
      }|] variables
    )

(**
 * [parse_grammar json] is the tuple [(t, v)] where [t] contains the token
 * records and [v] contains the variable records.
 *)
let parse_grammar json =
  let tokens = get_tokens json in
  let variables = get_variables json in
  (tokens, variables)

(**
 * [parse_args ()] parses the first argument to the executable that should
 * contain the file name containing the grammar JSON.
 *)
let parse_args () =
  try
    Sys.argv.(1)
  with _ ->
    print_endline "Invalid arguments: missing input file";
    exit 0

(**
 * [tokens_in_order] is the list of tokens in the order specified in the
 * grammar file.
 *
 * [variables_in_order] is the list of variables in the order specified in the
 * grammar file.
 *)
let (tokens_in_order, variables_in_order) =
  try
    Yojson.Basic.from_file "grammar.json" |> parse_grammar
  with Sys_error _ ->
    print_endline "Invalid arguments: invalid input file";
    exit 1

let num_tokens = Array.length tokens_in_order
let num_variables = Array.length variables_in_order
let start_variable = num_tokens
let last_variable = num_variables - 1 + start_variable
let first_set = Hashtbl.create num_variables;;
let variable_to_id_map =
  Hashtbl.create (num_tokens + num_variables);;

for idx = 0 to num_tokens - 1 do
  let token = Array.get tokens_in_order idx in
  Hashtbl.add variable_to_id_map token.name idx
done;;

for idx = start_variable to last_variable do
  let variable = Array.get variables_in_order (idx - start_variable) in
  Hashtbl.add variable_to_id_map variable.name idx
done;;

let is_token var_id =
  var_id < num_tokens

let get_variable_id var_name =
  Hashtbl.find variable_to_id_map var_name;;

let get_variable var_id =
  Array.get variables_in_order (var_id - start_variable);;

(* Step #1: Compute FIRST set for every variable. *)
for idx = start_variable to last_variable do
  let closure_set = Hashtbl.create num_tokens in
  let bfs = Queue.create () in
  let visited = Hashtbl.create num_variables in
  Queue.push idx bfs;
  Hashtbl.add visited idx true;
  while not (Queue.is_empty bfs) do
    let curr = Queue.pop bfs in
    if is_token curr then Hashtbl.add closure_set curr 0
    else begin
      let variable = get_variable curr in
      Array.iter (fun prod ->
        let hd = Array.get prod 0 in
        let hd_id = get_variable_id hd in
        if not (Hashtbl.mem visited hd_id) then begin
          Queue.push hd_id bfs;
          Hashtbl.add visited hd_id true;
        end
      ) variable.productions
    end
  done;
  Hashtbl.add first_set idx closure_set;
done;;

(* Step #2: Compute item set 0. *)
let closure initial_items =
  let prods = Queue.create () in
  let initial_set = Hashtbl.create (List.length initial_items) in
  let visited = Hashtbl.create num_variables in
  List.iter (fun item ->
    Queue.push item prods;
    Hashtbl.add visited item true;
    Hashtbl.add initial_set item true;
  ) initial_items;
  while not (Queue.is_empty prods) do
    let (var_id, prod_id, pos) = Queue.pop prods in
    let curr_var = get_variable var_id in
    let curr_prod = Array.get curr_var.productions prod_id in
    if pos < Array.length curr_prod then begin
      let next_var_name = Array.get curr_prod pos in
      let next_var_id = get_variable_id next_var_name in
      if not (is_token next_var_id) then begin
        let next_var = get_variable next_var_id in
        for prod_id = 0 to Array.length next_var.productions - 1 do
          if not (Hashtbl.mem visited (next_var_id, prod_id, 0)) then begin
            Hashtbl.add visited (next_var_id, prod_id, 0) true;
            Queue.push (next_var_id, prod_id, 0) prods;
            Hashtbl.remove initial_set (next_var_id, prod_id, 0)
          end
        done
      end
    end
  done;
  (
    Hashtbl.fold (fun item _ acc -> item::acc) initial_set [],
    Hashtbl.fold (fun item _ acc -> item::acc) visited []
  );;

let (initial, items) = closure [(start_variable, 0, 0)];;
List.iter (fun (a,b,c) ->
  print_endline (string_of_int a ^ "," ^ string_of_int b ^ "," ^ string_of_int c)
) initial
