type parse_tree =
  | Token of Token.t
  | Node of parse_tree list

(** 
 * [iterate_over_productions f] applies function [f] to each rule in the
 * grammar. 
 *
 * [f] should take in inputs of the form [(int a, int b, int c)], which 
 * represents the production rule [a -> b c] in the post CNF grammar where 
 * a, b, c are variable identifiers.
 *)
let iterate_over_productions f =
  List.iteri (fun a el ->
    let a = a + Grammar.num_tokens in
    List.iter (fun (b, c) ->
        f (a,b,c)
    ) el
  ) Grammar.rules

let parse tok_arr =
  let n = Array.length tok_arr in
  let dp = Array.init n
    (fun _ -> Array.init n (fun _ ->
      (Array.make Grammar.num_variables false))) in
  let prev = Array.init n
    (fun _ -> Array.init n (fun _ ->
      (Array.make Grammar.num_variables (-1, -1, -1)))) in
  for s = 0 to n - 1 do
    List.iter (fun v ->
        dp.(s).(s).(v) <- true;
        prev.(s).(s).(v) <- (-1, -1, -1)
      ) (Grammar.token_to_varid tok_arr.(s))
  done;

  for l = 1 to n - 1 do
    for s = 0 to n - 1 - l do
        for m = s to s + l - 1 do
          iterate_over_productions (fun (a, b, c) ->
            if not dp.(s).(s + l).(a) &&
                   dp.(s).(m).(b) &&
                   dp.(m + 1).(s + l).(c)
            then begin
              dp.(s).(s + l).(a) <- true;
              prev.(s).(s+l).(a) <- (b, m, c);
            end
          )
        done
    done
  done;

  if not dp.(0).(n - 1).(Grammar.start_variable)
  then failwith "Invalid program."
  else
    let rec generate_var_tree s e v =
      if (s = e) then Token (tok_arr.(s)) else
      let (left_var, middle_index, right_var) = prev.(s).(e).(v) in
      let left_parse_tree = generate_var_tree s middle_index left_var in
      let right_parse_tree = generate_var_tree (middle_index + 1) e right_var in
      let l_children =
        if Grammar.auto_generated_variable left_var then
          match left_parse_tree with
          | Token t -> [Token t]
          | Node lst -> lst
        else
          [left_parse_tree]
      in
      let children =
        if Grammar.auto_generated_variable right_var then
          match right_parse_tree with
          | Token t -> l_children@[Token t]
          | Node lst -> l_children@lst
        else
          l_children@[right_parse_tree]
      in Node (children)
    in
    generate_var_tree 0 (n - 1) Grammar.start_variable
