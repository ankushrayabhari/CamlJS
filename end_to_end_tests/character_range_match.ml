let temp = 'a';;

let is_alpha c = match c with
  | 'a'..'z' -> true
  | 'A'..'Z' -> true
  | _ -> false;;

print_endline (string_of_bool (is_alpha 'x'));
print_endline (string_of_bool (is_alpha '1'));
print_endline (string_of_bool (is_alpha '\n'));
print_endline (string_of_bool (is_alpha 'K'));;
