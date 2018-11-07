let match_tuples t = match t with
  | (x, _, 20) -> print_string (string_of_int x)
  | (1, 2, y) -> print_string (string_of_int y)
  | (_, _, _) as t -> print_endline "LOL";;

match_tuples (0,2,20);;
match_tuples (~-1, 50, 90);;
match_tuples (1,2,20);;
