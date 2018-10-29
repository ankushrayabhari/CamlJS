open Yojson.Basic
open Yojson.Basic.Util
open Printf

type token = {
  name: string;
  regex: string;
  tag: string option;
  parameter: string option;
}

type variable = {
  name: string;
  productions: string list list;
}

let parse_token json =
  {
    name = member "name" json |> to_string;
    regex = member "regex" json |> to_string;
    tag = member "tag" json  |> to_string_option;
    parameter = member "parameter" json  |> to_string_option;
  }

let parse_variable (n, p) = {
  name = n;
  productions =
    p
    |> to_list
    |> List.map (fun el -> el |> to_list |> List.map to_string)
}

let get_tokens grammar_json =
  member "tokens" grammar_json
  |> to_list
  |> List.map parse_token

let get_variables grammar_json =
  member "productions" grammar_json
  |> to_assoc
  |> List.map parse_variable

let to_hash lst = List.fold_left
  (fun (acc, idx) el -> Hashtbl.add acc idx el;(acc, idx + 1))
  (Hashtbl.create (List.length lst), 0)
  lst

let parse_grammar json =
  let tokens = get_tokens json in
  let variables = get_variables json in
  (tokens, variables)

let parse_args () =
  try
    Sys.argv.(1)
  with _ ->
    print_endline "Invalid arguments: missing input file";
    exit 0

(* Parse grammar into tokens and variables. *)
let (tokens_in_order, variables_in_order) =
  try
    Yojson.Basic.from_file (parse_args ()) |> parse_grammar
  with Sys_error _ ->
    print_endline "Invalid arguments: invalid input file";
    exit 1

let tokens = tokens_in_order |> to_hash |> fst
let variables = variables_in_order |> to_hash |> fst
let num_tokens = Hashtbl.length tokens
let last_variable = Hashtbl.length variables - 1 + num_tokens
let start_variable = num_tokens
let num_variables = ref (Hashtbl.length variables + num_tokens)

(* Parse tokens and variables into hash map from variable name to id. *)
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

let get_variable_id name = Hashtbl.find variable_to_id_map name

let is_unit_production production =
  List.length production = 1 &&
  get_variable_id (List.hd production) >= num_tokens

let get_productions var_id =
  if var_id < num_tokens then []
  else (Hashtbl.find variables (var_id - num_tokens)).productions

let set_productions var_id new_prod =
  let no_duplicates_productions = List.sort_uniq compare new_prod in
  let var = Hashtbl.find variables (var_id - num_tokens) in
  Hashtbl.replace variables (var_id - num_tokens) {
    var with productions = no_duplicates_productions;
  }

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

let is_large_production prod =
  Stack.length prod > 2

let is_token_production prod =
  List.length prod = 1 && get_variable_id (List.hd prod) < num_tokens

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

let start_variable_int () =
  sprintf "let start_variable = %d\n" start_variable;;

let num_tokens_int () =
  sprintf "let num_tokens = %d\n" num_tokens;;

let num_variables_int () =
  sprintf "let num_variables = %d\n" !num_variables;;

let auto_generated_variable () =
  sprintf "let auto_generated_variable var = var > %d\n" last_variable;;

let token_decl () =
  List.map (fun el ->
    match el.parameter with
    | Some p -> sprintf "  | %s of %s\n" el.name p
    | None -> sprintf "  | %s\n" el.name
  ) tokens_in_order
  |> String.concat ""
  |> sprintf "type t =\n%s";;

let tokenize_sig () = {|
val tokenize : string -> Token.t list

val has_tag : Token.t -> string -> bool

val token_to_string : Token.t -> string
|};;

let regexp_of_token_fn () =
  List.map (fun el ->
    let output_regex = Str.global_replace (Str.regexp "\\") {|\\\\|} el.regex in
    match el.parameter with
    | Some p -> sprintf "  | %s _ -> \"%s\"\n" el.name output_regex
    | None -> sprintf "  | %s -> \"%s\"\n" el.name output_regex
  ) tokens_in_order
  |> String.concat ""
  |> sprintf "let regexp_of_token tok = Str.regexp (match tok with\n%s)\n";;

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

let tokenize_impl = {|
let rec tokenize_rec str start tok_lst =
  let whitespace_regex = Str.regexp "[ \n\r\t]*" in
  let _ = Str.string_match whitespace_regex str start in
  let start_index = start + (String.length (Str.matched_string str)) in
  if start_index >= (String.length str) then tok_lst |> List.rev
  else
    let token = List.fold_left (fun acc curr_tok -> match acc with
        | (Some _, _) -> acc
        | (None, _) ->
          let regex = regexp_of_token curr_tok in
          if Str.string_match regex str start_index
          then
            let matched_str = Str.matched_string str in
            let len = String.length matched_str in
            (Some (parametrize_tok matched_str curr_tok), len)
          else (None, -1)
      ) (None, -1) precedence
    in
    match token with
    | (Some tok, len) -> tokenize_rec str (start_index + len) (tok::tok_lst)
    | (None, _) ->
        failwith ("Unknown symbol at " ^ string_of_int start_index ^ ".")

let tokenize str =
  tokenize_rec str 0 []
|};;

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

let token_to_string_fn () =
  List.map (fun el ->
    match el.parameter with
    | Some _ -> sprintf "  | %s _ -> \"%s\"\n" el.name el.name
    | None -> sprintf "  | %s -> \"%s\"\n" el.name el.name
  ) tokens_in_order
  |> String.concat ""
  |> sprintf "let token_to_string = function\n%s";;

let header = {|(* DO NOT UPDATE THIS FILE! *)
(* Update grammar.json and then run make grammar! *)
|};;

let grammar_text =
  sprintf "%s\n%s\n%s\n%s\n%s\n%s\n%s"
    header
    (token_to_varid_fn ())
    (rules_lst ())
    (start_variable_int ())
    (num_tokens_int ())
    (num_variables_int ())
    (auto_generated_variable ());;

let token_mli_text =
  sprintf "%s\n%s"
    header
    (token_decl ());;

let tokenizer_mli_text =
  sprintf "%s%s"
    header
    (tokenize_sig ());;

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

let write_to_file f txt =
  let oc = open_out f in
  fprintf oc "%s" txt;
  close_out oc;
  print_endline (f ^ " updated!");;

write_to_file "grammar.ml" grammar_text;;
write_to_file "token.mli" token_mli_text;;
write_to_file "tokenizer.mli" tokenizer_mli_text;;
write_to_file "tokenizer.ml" tokenizer_text;;
