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
  productions: string list list;
}

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
    |> List.map (fun el -> el |> to_list |> List.map to_string)
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
  |> List.map parse_token

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

(**
 * [to_hash lst] is the [Hashtbl] mapping from element index to the element.
 * Each element of [lst] will be contained in the resulting map.
 *)
let to_hash lst = List.fold_left
  (fun (acc, idx) el -> Hashtbl.add acc idx el;(acc, idx + 1))
  (Hashtbl.create (List.length lst), 0)
  lst

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
    Yojson.Basic.from_file (parse_args ()) |> parse_grammar
  with Sys_error _ ->
    print_endline "Invalid arguments: invalid input file";
    exit 1

(**
 * [tokens] is the index to token map of [tokens_in_order].
 *
 * {b See:} to_hash for the table definition of [tokens]
 *)
let tokens = tokens_in_order |> to_hash |> fst

(**
 * [variables] is the index to variable map of [variables_in_order].
 *
 * {b See:} to_hash for the table definition of [variables]
 *)
let variables = variables_in_order |> to_hash |> fst

(** [num_tokens] is the number of tokens. *)
let num_tokens = Hashtbl.length tokens
(** [last_variable] is the last user provided variable number. *)
let last_variable = Hashtbl.length variables - 1 + num_tokens
(** [start_variable] is the number of the start variable of the grammar. *)
let start_variable = num_tokens
(** [num_variables] is total number of variables. *)
let num_variables = ref (Hashtbl.length variables + num_tokens)

(** [variable_to_id_map] maps from the variable identifier to its number. *)
let variable_to_id_map =
  Hashtbl.create (Hashtbl.length tokens + Hashtbl.length variables);;

for idx = 0 to num_tokens - 1 do
  let token = Hashtbl.find tokens idx in
  Hashtbl.add variable_to_id_map token.name idx
done;;

for idx = start_variable to last_variable do
  let variable = Hashtbl.find variables (idx - num_tokens) in
  Hashtbl.add variable_to_id_map variable.name idx
done;;

(**
 * [get_variable_id name] is the variable number of the variable with
 * identifier [name].
 *)
let get_variable_id name = Hashtbl.find variable_to_id_map name

(**
 * [is_unit_production production] is whether or not the [production] is of the
 * form [V]{_ [i]} [->] [V]{_ [j]} where [V]{_ [j]} is not a token.
 *)
let is_unit_production production =
  List.length production = 1 &&
  get_variable_id (List.hd production) >= num_tokens

(**
 * [get_productions var_id] is the list of productions of the variable with
 * number [var_id].
 *)
let get_productions var_id =
  if var_id < num_tokens then []
  else (Hashtbl.find variables (var_id - num_tokens)).productions

(**
 * [set_productions var_id new_prod] updates the variable with number [var_id]
 * to have productions [new_prod].
 *)
let set_productions var_id new_prod =
  let no_duplicates_productions = List.sort_uniq compare new_prod in
  let var = Hashtbl.find variables (var_id - num_tokens) in
  Hashtbl.replace variables (var_id - num_tokens) {
    var with productions = no_duplicates_productions;
  }

(**
 * [add_variable var1 var2] creates a new variable with a production to
 * [var1] concatenated with [var2]. It evaluates to the idenfitier of the new
 * variable.
 *)
let add_variable var_1_name var_2_name =
  let new_var = {
    name =
      var_1_name ^ "_" ^
      var_2_name ^ "_" ^
      (string_of_int !num_variables);
    productions = [[var_1_name; var_2_name]];
  } in
  Hashtbl.add variables (Hashtbl.length variables) new_var;
  Hashtbl.add variable_to_id_map new_var.name !num_variables;
  incr num_variables;
  new_var.name

(**
 * [is_large_production prod] is whether or not [prod] is considered a large
 * production (i.e. has more than two terminal or variable symbols).
 *)
let is_large_production prod =
  Stack.length prod > 2

(**
 * [is_token_production prod] is whether or not [prod] is considered a large
 * production (i.e. has more than two terminal or variable symbols).
 *)
let is_token_production prod =
  List.length prod = 1 && get_variable_id (List.hd prod) < num_tokens

(**
 * [remove_large_productions ()] decomposes all productions [p] such that
 * [is_large_production p] is [true] into smaller productions.
 *
 * It does this by repeatedly replacing the last two symbols of the production
 * with a new variable that has a production to those symbols until it is no
 * longer large.
 *)
let remove_large_productions () =
  for var_id = start_variable to last_variable do
    get_productions var_id
    |> List.map (fun el ->
      let production =
        el
        |> List.fold_left
            (fun acc el -> Stack.push el acc; acc)
            (Stack.create ())
      in
      while is_large_production production do
        let last_el = Stack.pop production in
        let second_last_el = Stack.pop production in
        let new_variable = add_variable second_last_el last_el in
        Stack.push new_variable production
      done;
      Stack.fold (fun lst var -> var::lst) [] production
      )
    |> set_productions var_id
  done

(**
 * [remove_unit_productions var_id] recursively removes all unit productions
 * from [var_id] by copying all productions of its children unit production
 * variables.
 *
 * {b Requires:} There must not be any unit production cycles in the grammar.
 *)
let rec remove_unit_productions var_id =
  let to_add_productions = ref [] in
  let to_remove = Hashtbl.create 10 in
  get_productions var_id
  |> List.filter is_unit_production
  |> List.iter (fun unit_production ->
      let sub_variable = List.hd unit_production in
      let sub_variable_id = get_variable_id sub_variable in
      Hashtbl.add to_remove sub_variable ();
      remove_unit_productions sub_variable_id;
      get_productions sub_variable_id
      |> List.iter (fun prod ->
          to_add_productions := prod::!to_add_productions
        )
    );
  let new_productions =
    (get_productions var_id)@(!to_add_productions)
    |> List.filter (fun el ->
        not (List.length el = 1 && Hashtbl.mem to_remove (List.hd el))
      )
  in
  set_productions var_id new_productions;;

remove_large_productions ();;

for var_id = start_variable to last_variable do
  remove_unit_productions var_id
done;;

(**
 * [token_to_varid_fn ()] is the OCaml code of the [token_to_varid] function.
 *
 * {b See:} Grammar for documentation of this method.
 *)
let token_to_varid_fn () =
  let tokenToVarId = Hashtbl.create (Hashtbl.length tokens) in
  for var_id = 0 to num_tokens - 1 do
    Hashtbl.add tokenToVarId (Hashtbl.find tokens var_id).name [var_id];
  done;
  for var_id = start_variable to last_variable do
    get_productions var_id
    |> List.filter is_token_production
    |> List.iter (fun production ->
        let name = List.hd production in
        let lst = Hashtbl.find tokenToVarId name in
        Hashtbl.replace tokenToVarId name (var_id::lst)
      )
  done;
  List.mapi (fun idx (token: token)  ->
    sprintf "  | %s %s-> [%s]\n"
      token.name
      (match token.parameter with None -> "" | Some _ -> "_ ")
      (
        Hashtbl.find tokenToVarId token.name
        |> List.map string_of_int
        |> List.rev
        |> String.concat ";"
      )
  ) tokens_in_order
  |> String.concat ""
  |> sprintf "let token_to_varid = Token.(function\n%s)\n";;

(**
 * [rules_lst ()] is the OCaml code of the [rules] list.
 *
 * {b See:} Grammar for documentation of this property.
 *)
let rules_lst () =
  let rules = ref "" in
  for var_id = start_variable to !num_variables - 1 do
    get_productions var_id
    |> List.filter (fun el -> not (is_token_production el))
    |> List.map (fun prod ->
        List.map get_variable_id prod
       )
    |> List.sort compare
    |> List.mapi (fun idx production ->
        let endline = if idx > 0 && idx mod 7 = 0 then "\n   " else "" in
        sprintf "(%s); %s"
          (
            production
            |> List.map string_of_int
            |> String.concat ","
          )
          endline
      )
    |> String.concat ""
    |> (fun rule -> sprintf "\n  [%s]; (* %d *)" rule var_id)
    |> (fun rule -> rules := !rules ^ rule)
  done;
  sprintf "let rules = [%s\n]\n" !rules;;

(**
 * [start_variable_int ()] is the OCaml code of the [start_variable] number.
 *
 * {b See:} Grammar for documentation of this property.
 *)
let start_variable_int () =
  sprintf "let start_variable = %d\n" start_variable;;

(**
 * [num_tokens_int ()] is the OCaml code of the [num_tokens] number.
 *
 * {b See:} Grammar for documentation of this property.
 *)
let num_tokens_int () =
  sprintf "let num_tokens = %d\n" num_tokens;;

(**
 * [num_variables_int ()] is the OCaml code of the [num_tokens] number.
 *
 * {b See:} Grammar for documentation of this property.
 *)
let num_variables_int () =
  sprintf "let num_variables = %d\n" !num_variables;;

(**
 * [auto_generated_variable ()] is the OCaml code of the
 * [auto_generated_variable] function.
 *
 * {b See:} Grammar for documentation of this method.
 *)
let auto_generated_variable () =
  sprintf "let auto_generated_variable var = var > %d\n" last_variable;;

(**
 * [token_decl ()] is the OCaml code of the [Token.t] variant.
 *
 * {b See:} Token for documentation of this type.
 *)
let token_decl () =
  List.map (fun el ->
    match el.parameter with
    | Some p -> sprintf "  | %s of %s\n" el.name p
    | None -> sprintf "  | %s\n" el.name
  ) tokens_in_order
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
 * position and then recurse on the rest of the stirng.
 * Ties are resolved in the order that the tokens were defined.
 *
 * {b Requires}:
 * - [program] is a valid program according to the grammar defined in
 * grammar.json.
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
  List.map (fun el ->
    let output_regex = String.escaped el.regex in
    match el.parameter with
    | Some p -> sprintf "  | %s _ -> \"%s\"\n" el.name output_regex
    | None -> sprintf "  | %s -> \"%s\"\n" el.name output_regex
  ) tokens_in_order
  |> String.concat ""
  |> sprintf "let regexp_of_token tok = Str.regexp (match tok with\n%s)\n";;

(**
 * [precedence_arr ()] is the OCaml code of the [precedence] list of tokens.
 *
 * {b See:} Tokenizer for documentation of this property.
 *)
let precedence_arr () =
  List.map (fun el ->
    match el.parameter with
    | Some "int" -> sprintf "  %s 0;\n" el.name
    | Some "string" -> sprintf "  %s \"\";\n" el.name
    | Some "float" -> sprintf "  %s 0.0;\n" el.name
    | Some "bool" -> sprintf "  %s false;\n" el.name
    | Some "char" -> sprintf "  %s 'a';\n" el.name
    | None -> sprintf "  %s;\n" el.name
    | _ -> failwith "unknown parameter type"
  ) tokens_in_order
  |> String.concat ""
  |> sprintf "let precedence = [\n%s]\n";;

(**
 * [parametrize_tok_fn ()] is the OCaml code of the [parametrize_tok] function.
 *
 * {b See:} Tokenizer for documentation of this method.
 *)
let parametrize_tok_fn () =
  List.filter (fun el -> el.parameter != None) tokens_in_order
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
  |> sprintf "let parametrize_tok str = function\n%s  | t -> t\n";;

(**
 * [tokenize_impl ()] is the OCaml code of the [tokenize] function.
 *
 * {b See:} Tokenizer for documentation of this method.
 *)
let tokenize_impl = {|
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
  List.filter (fun el -> el.tag != None) tokens_in_order
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
  List.map (fun el ->
    match el.parameter with
    | Some _ -> sprintf "  | %s _ -> \"%s\"\n" el.name el.name
    | None -> sprintf "  | %s -> \"%s\"\n" el.name el.name
  ) tokens_in_order
  |> String.concat ""
  |> sprintf "let token_to_string = function\n%s";;

(**
 * [header] is the warning comment telling not to change any autogenerated file.
 *)
let header = {|(* DO NOT UPDATE THIS FILE! *)
(* Update grammar.json and then run make grammar! *)
|};;

let token_mli_header = "(** The definition of all the tokens. *)";;
let tokenizer_mli_text = "(** The interface of the tokenizer. *)";;

(**
 * [grammar_text] is the content of the grammar metadata file.
 *)
let grammar_text =
  sprintf "%s\n%s\n%s\n%s\n%s\n%s\n%s"
    header
    (token_to_varid_fn ())
    (rules_lst ())
    (start_variable_int ())
    (num_tokens_int ())
    (num_variables_int ())
    (auto_generated_variable ());;

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

write_to_file "grammar.ml" grammar_text;;
write_to_file "token.mli" token_mli_text;;
write_to_file "tokenizer.mli" tokenizer_mli_text;;
write_to_file "tokenizer.ml" tokenizer_text;;
