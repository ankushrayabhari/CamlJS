let temp = ['a'; 'b'; '\n'; '\t'; 'l'; '\n']

let rec print_chars tr = match tr with
  | h::t -> print_string (String.make 1 (Char.uppercase_ascii h));print_chars t
  | [] -> 0;;

print_chars temp;;
