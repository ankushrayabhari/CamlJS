let rec nacci n =
  match n with
  | 1 -> 1
  | 2 -> 1
  | n -> nacci (n - 1) + nacci (n - 2);;

print_endline (string_of_int (nacci 10))
