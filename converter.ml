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
  |> Array.append [|
    {
      name = "EOF'";
      regex = "";
      tag = None;
      parameter = None;
    }
    |]

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
        name = "START'";
        productions = [|[|
          hd.name; "EOF'"
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

let empty_token = 0;;
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
for idx = 0 to num_tokens - 1 do
  let token_set = Hashtbl.create 1 in
  Hashtbl.add token_set idx true;
  Hashtbl.add first_set idx token_set
done;;

for idx = start_variable to last_variable do
  let closure_set = Hashtbl.create num_tokens in
  let bfs = Queue.create () in
  let visited = Hashtbl.create num_variables in
  Queue.push idx bfs;
  Hashtbl.add visited idx true;
  while not (Queue.is_empty bfs) do
    let curr = Queue.pop bfs in
    if is_token curr then Hashtbl.add closure_set curr true
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
    let (var_id, prod_id, pos, lookahead) = Queue.pop prods in
    let curr_var = get_variable var_id in
    let curr_prod = Array.get curr_var.productions prod_id in
    if pos < Array.length curr_prod then begin
      let next_var_name = Array.get curr_prod pos in
      let next_var_id = get_variable_id next_var_name in
      let next_next_first_set =
        if pos + 1 < Array.length curr_prod then begin
          let next_next_var_name = Array.get curr_prod (pos + 1) in
          let next_next_var_id = get_variable_id next_next_var_name in
          Hashtbl.find first_set next_next_var_id
        end
        else if lookahead = empty_token then Hashtbl.create 0
        else Hashtbl.find first_set lookahead
      in
      if not (is_token next_var_id) then begin
        let next_var = get_variable next_var_id in
        for prod_id = 0 to Array.length next_var.productions - 1 do
          Hashtbl.iter (fun tok _ ->
            if not (Hashtbl.mem visited (next_var_id, prod_id, 0, tok))
            then begin
              Hashtbl.add visited (next_var_id, prod_id, 0, tok) true;
              Queue.push (next_var_id, prod_id, 0, tok) prods;
              Hashtbl.remove initial_set (next_var_id, prod_id, 0, tok)
            end
          ) next_next_first_set
        done
      end
    end
  done;
  (
    Hashtbl.fold (fun item _ acc -> item::acc) initial_set [],
    Hashtbl.fold (fun item _ acc -> item::acc) visited []
  );;

let (item_set_0_canonical, item_set_0_items) =
  closure [(start_variable, 0, 0, empty_token)];;
let item_sets = Hashtbl.create 10000;;
let reverse_item_set_lookup = Hashtbl.create 10000;;
let item_set_id = ref 0;;

Hashtbl.add item_sets !item_set_id (item_set_0_canonical, item_set_0_items);;
Hashtbl.add reverse_item_set_lookup item_set_0_canonical !item_set_id;;
incr item_set_id;;

let transitions = Hashtbl.create 10000;;
let item_set_construction = Queue.create ();;
let visited = Hashtbl.create 10000;;
Queue.push 0 item_set_construction;;
Hashtbl.add visited 0 true;;
while not (Queue.is_empty item_set_construction) do
  let curr_item_set_id = Queue.pop item_set_construction in
  let (canonical, items) = Hashtbl.find item_sets curr_item_set_id in
  List.map (fun ((var_id, prod_id, pos, lookahead) as item) ->
    let curr_var = get_variable var_id in
    let curr_prod = Array.get curr_var.productions prod_id in
    if pos < Array.length curr_prod then begin
      let next_var_name = Array.get curr_prod pos in
      (item, get_variable_id next_var_name)
    end
    else (item, empty_token)
  ) items
  |> List.filter (fun (_, o) -> o <> empty_token)
  |> List.sort (fun (_, o1) (_, o2) -> compare o1 o2)
  |> List.fold_left (fun (lst, last_next_id) (item, next_id) ->
      if last_next_id = next_id then begin
        match lst with
        | (h, _)::t -> ((item::h, next_id)::t, next_id)
        | [] -> ([([item], next_id)], next_id)
      end else
      (([item], next_id)::lst, next_id)
     ) ([], -1)
  |> fst
  |> List.iter (fun (item_group, next_id) ->
      let advanced_items = List.map (fun (var_id, prod_id, pos, lookahead) ->
        (var_id, prod_id, pos + 1, lookahead)
      ) item_group in
      let (canonical, items) = closure advanced_items in
      try begin
        let id = Hashtbl.find reverse_item_set_lookup canonical in
        (* add transition from curr_item_set_id to id on next_id *)
        Hashtbl.add transitions (curr_item_set_id, next_id) id
      end with _ -> begin
        Hashtbl.add item_sets !item_set_id (canonical, items);
        Hashtbl.add reverse_item_set_lookup canonical !item_set_id;
        Hashtbl.add transitions (curr_item_set_id, next_id) !item_set_id;
        Queue.push !item_set_id item_set_construction;
        incr item_set_id;
      end
    )
done;;

type action =
  | Shift of int
  | Reduce of int * int
  | Accept
  | Goto of int
  | Error;;

let last_item = !item_set_id - 1;;
let num_items = !item_set_id;;

let action_table = Array.init num_items (fun _ ->
  Array.make (num_variables + num_tokens) Error);;

Hashtbl.iter (fun (from_state, tok) to_state ->
  action_table.(from_state).(tok) <- Goto to_state
) transitions;;

for item_id = 0 to last_item do
  let (_, items) = Hashtbl.find item_sets item_id in
  List.iter (fun (var_id, prod_id, pos, lookahead) ->
    let var = get_variable var_id in
    let prod = Array.get var.productions prod_id in
    let next_id = if pos < Array.length prod then begin
      let next_name = Array.get prod pos in
      get_variable_id next_name
    end else empty_token in
    if next_id = empty_token then begin
      if var.name = "_START" && lookahead = empty_token then
        action_table.(item_id).(empty_token) <- Accept
      else
        action_table.(item_id).(lookahead) <- Reduce (var_id, prod_id)
    end else begin
      let to_state = Hashtbl.find_opt transitions (item_id, next_id) in
      match to_state with
      | None -> ()
      | Some new_state ->
        if is_token next_id
        then action_table.(item_id).(next_id) <- Shift new_state
    end
  ) items;
done;;


(**
 * [token_decl ()] is the OCaml code of the [Token.t] variant.
 *
 * {b See:} Token for documentation of this type.
 *)
let token_decl () =
  Array.map (fun el ->
    match el.parameter with
    | Some p -> sprintf "  | %s of %s\n" el.name p
    | None -> sprintf "  | %s\n" el.name
  ) tokens_in_order
  |> Array.to_list
  |> String.concat ""
  |> sprintf "type t =\n%s";;

(**
 * [tokenize_sig ()] is the OCaml code of the signature for Tokenizer.
 *
 * {b See:} Tokenizer for documentation of these methods.
 *)
let tokenize_sig () = {|
(**
 * [tokenize program] is the list of tokens that make up [program].
 *
 * It will greedily pick the largest token that matches any given starting
 * position and then recurse on the rest of the string.
 *
 * Ties are resolved in the order that the tokens were defined.
 *
 * @raise Failure if [program] contains a substring at a valid token start index
 * from the greedy strategy that does not match any known token.
 *)
val tokenize : string -> Token.t array

(**
 * [has_tag tok tag] is whether [tok] has the tag [tag] according
 * to its definition in grammar.
 *)
val has_tag : Token.t -> string -> bool

(**
 * [token_to_string tok] is the string representation of [tok].
 *)
val token_to_string : Token.t -> string
|};;

(**
 * [regexp_of_token_fn ()] is the OCaml code of the [regexp_of_token] function.
 *
 * {b See:} Tokenizer for documentation of these methods.
 *)
let regexp_of_token_fn () =
  Array.map (fun el ->
    let output_regex = String.escaped el.regex in
    match el.parameter with
    | Some p -> sprintf "  | %s _ -> \"%s\"\n" el.name output_regex
    | None -> sprintf "  | %s -> \"%s\"\n" el.name output_regex
  ) tokens_in_order
  |> Array.to_list
  |> String.concat ""
  |> sprintf {|
(**
 * [regexp_of_token tok] is the regular expression of [tok] as defined in
 * the grammar file.
 *)
let regexp_of_token tok = Str.regexp (match tok with
%s)
|};;

(**
 * [precedence_arr ()] is the OCaml code of the [precedence] list of tokens.
 *
 * {b See:} Tokenizer for documentation of this property.
 *)
let precedence_arr () =
  Array.map (fun el ->
    match el.parameter with
    | Some "int" -> sprintf "  %s 0;\n" el.name
    | Some "string" -> sprintf "  %s \"\";\n" el.name
    | Some "float" -> sprintf "  %s 0.0;\n" el.name
    | Some "bool" -> sprintf "  %s false;\n" el.name
    | Some "char" -> sprintf "  %s 'a';\n" el.name
    | None -> sprintf "  %s;\n" el.name
    | _ -> failwith "unknown parameter type"
  ) tokens_in_order
  |> Array.to_list
  |> String.concat ""
  |> sprintf {|
(**
 * [precedence] is the precedence (preferred tokens match order) of the tokens
 * in grammar.json, sorted by order of highest to lowest precendence.
 *
 * This corresponds to the order that tokens are defined in the grammar file.
 *)
let precedence = [
%s]
|};;

(**
 * [parametrize_tok_fn ()] is the OCaml code of the [parametrize_tok] function.
 *
 * {b See:} Tokenizer for documentation of this method.
 *)
let parametrize_tok_fn () =
  Array.to_list tokens_in_order
  |> List.filter (fun el -> el.parameter != None)
  |> List.map (fun el ->
      match el.parameter with
      | Some "int" ->
        sprintf "  | %s _ -> %s (int_of_string str)\n" el.name el.name
      | Some "string" ->
        sprintf "  | %s _ -> %s str\n" el.name el.name
      | Some "float" ->
        sprintf "  | %s _ -> %s (float_of_string str)\n" el.name el.name
      | Some "bool" ->
        sprintf "  | %s _ -> %s (bool_of_string str)\n" el.name el.name
      | Some "char" ->
        sprintf "  | %s _ -> %s (String.get str 0)\n" el.name el.name
      | _ -> failwith "should not be called on None"
    )
  |> String.concat ""
  |> sprintf {|
(**
 * [parametrize_tok val_str tok] updates [tok] to contain the data in [val_str].
 * - If a token has no parameter, the function does nothing but evaluate to the
 * token.
 * - The type of conversion performed by the method depends on the parameter
 * type of the token as defined in the grammar file.
 *)
let parametrize_tok str = function
%s  | t -> t
|};;

(**
 * [tokenize_impl ()] is the OCaml code of the [tokenize] function.
 *
 * {b See:} Tokenizer for documentation of this method.
 *)
let tokenize_impl = {|
(**
 * [tokenize_rec str start tok_lst] is the list of tokens following the
 * strategy defined in [tokenize] starting at the index [start] in [str].
 *
 * @raise Failure if at any valid starting index the string does not match
 * any token regular expressions.
 *)
let rec tokenize_rec str start tok_lst =
  let whitespace_regex = Str.regexp "[ \n\r\t]*" in
  let _ = Str.string_match whitespace_regex str start in
  let start_index = start + (String.length (Str.matched_string str)) in
  if start_index >= (String.length str) then tok_lst |> List.rev
  else
    let matched_tokens =
      List.map (fun curr_tok ->
        let regex = regexp_of_token curr_tok in
        if Str.string_match regex str start_index
        then
          let matched_str = Str.matched_string str in
          if matched_str = "" then None else
          Some (parametrize_tok matched_str curr_tok, String.length matched_str)
        else None
      ) precedence
      |> List.filter (fun el -> el <> None)
      |> List.stable_sort (fun el1 el2 ->
          match (el1, el2) with
          | (Some (_, len1), Some (_, len2)) -> compare len2 len1
          | _ -> failwith "should not contain none"
        )
    in
    match matched_tokens with
    | Some (tok, len)::t -> tokenize_rec str (start_index + len) (tok::tok_lst)
    | _ ->
        failwith ("Unknown symbol at " ^ string_of_int start_index ^ ".")

let tokenize str =
  tokenize_rec str 0 [] |> Array.of_list
|};;

(**
 * [has_tag_fn ()] is the OCaml code of the [has_tag] function.
 *
 * {b See:} Tokenizer for documentation of this method.
 *)
let has_tag_fn () =
  Array.to_list tokens_in_order
  |> List.filter (fun el -> el.tag != None)
  |> List.map (fun el ->
      let pattern = match el.parameter with
        | Some _ -> " _, "
        | None -> ", "
      in
      match el.tag with
      | Some tag -> sprintf "  | (%s%s\"%s\")" el.name pattern tag
      | None -> failwith "should not be called with None"
    )
  |> String.concat "\n"
  |> sprintf
    "let has_tag tok tag = match (tok, tag) with\n%s -> true\n  | _ -> false\n"
  ;;

(**
 * [token_to_string_fn ()] is the OCaml code of the [token_to_string] function.
 *
 * {b See:} Tokenizer for documentation of this method.
 *)
let token_to_string_fn () =
  Array.map (fun el ->
    match el.parameter with
    | Some _ -> sprintf "  | %s _ -> \"%s\"\n" el.name el.name
    | None -> sprintf "  | %s -> \"%s\"\n" el.name el.name
  ) tokens_in_order
  |> Array.to_list
  |> String.concat ""
  |> sprintf "let token_to_string = function\n%s";;

(**
 * [header] is the warning comment telling not to change any autogenerated file.
 *)
let header = {|(* DO NOT UPDATE THIS FILE! *)
(* Update grammar.json and then run make grammar! *)
|};;

(**
 * The documentation comment for token.mli
 *)
let token_mli_header = "(** The definition of every token. *)";;

(**
 * The documentation comment for tokenizer.mli
 *)
let tokenizer_mli_text = "(** Converts an input string to a token array. *)";;

(**
 * [token_mli_text] is the content of token inteface file.
 *)
let token_mli_text =
  sprintf "%s\n%s\n%s"
    token_mli_header
    header
    (token_decl ());;

(**
 * [tokenizer_mli_text] is the content of tokenizer inteface file.
 *)
let tokenizer_mli_text =
  sprintf "%s\n%s%s"
    tokenizer_mli_text
    header
    (tokenize_sig ());;

(**
 * [tokenizer_text] is the content of tokenizer implementation file.
 *)
let tokenizer_text =
  sprintf "%s%s\n%s\n%s\n%s%s\n%s\n%s"
    header
    "open Token\n"
    (regexp_of_token_fn ())
    (precedence_arr ())
    (parametrize_tok_fn ())
    tokenize_impl
    (has_tag_fn ())
    (token_to_string_fn ());;

(**
 * [write_to_file f txt] writes [txt] to a file with name [f] and prints a
 * success message to standard out.
 *)
let write_to_file f txt =
  let oc = open_out f in
  fprintf oc "%s" txt;
  close_out oc;
  print_endline (f ^ " updated!");;

write_to_file "token.mli" token_mli_text;;
write_to_file "tokenizer.mli" tokenizer_mli_text;;
write_to_file "tokenizer.ml" tokenizer_text;;
