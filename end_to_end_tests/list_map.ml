let temp = ['a'; 'b'; '\n'; '\t'; 'l'; '\n']

let rec print_chars tr = match tr with
  | h::t -> print_char h;print_chars t
  | [] -> 0;;

print_chars temp;;
