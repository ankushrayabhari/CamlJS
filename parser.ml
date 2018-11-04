open Lr_action
open Parse_tree

type stack_data =
  | State of int
  | ParseTree of Parse_tree.t

let rec remove_unit_productions tr =
  let updated_tr = match tr with
  | Token t -> Token t
  | Node lst -> Node (List.map remove_unit_productions lst)
  in
  match updated_tr with
  | Node [Node lst] -> Node lst
  | Node [Token tr] -> Token tr
  | _ -> updated_tr

let parse tok_arr =
  let tok_arr = Array.append tok_arr [|Token.EOF'; Token.EMPTY';|] in
  let tok_arr_hd = ref 0 in
  let stck = Stack.create () in
  Stack.push (State 0) stck;
  let ret_val = ref (Node []) in
  let accept_reached = ref false in
  while not !accept_reached do
    let curr_state = match Stack.top stck with
      | ParseTree _ -> failwith "should not be a parse tree"
      | State s -> s
    in
    let lookahead = Grammar.token_to_var_id tok_arr.(!tok_arr_hd) in
    match Grammar.action_table.(curr_state).(lookahead) with
    | Shift s -> begin
        Stack.push (ParseTree (Token tok_arr.(!tok_arr_hd))) stck;
        incr tok_arr_hd;
        Stack.push (State s) stck
    end
    | Reduce (prod_size, lhs) -> begin
        let top_els = Stack.create () in
        for i = 1 to prod_size do
          ignore (Stack.pop stck);
          match Stack.pop stck with
          | State _ -> failwith "Should not be a state"
          | ParseTree tr -> Stack.push tr top_els
        done;
        let lst = Stack.fold (fun acc el -> el::acc) [] top_els |> List.rev in
        let curr_state = match Stack.top stck with
          | ParseTree _ -> failwith "should not be a parse tree"
          | State s -> s
        in
        match Grammar.action_table.(curr_state).(lhs) with
        | Goto s ->
          Stack.push (ParseTree (Node lst)) stck;
          Stack.push (State s) stck
        | _ -> failwith "should not be anything besides Goto"
    end
    | Accept -> begin
      ignore (Stack.pop stck);
      ignore (Stack.pop stck);
      ignore (Stack.pop stck);
      match Stack.top stck with
        | ParseTree tr -> begin
          ret_val := tr;
          accept_reached := true
        end
        | State s -> failwith "Invalid Program"
    end
    | _ -> failwith "Invalid Program."
  done;
  !ret_val |> remove_unit_productions
