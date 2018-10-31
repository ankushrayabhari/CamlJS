let lst1 = [1;2;3;4;5]
let lst2 = 1::2::3::4::5::[]
let lst3 = [1]@[2;3;4]

let two_or_four_elements = function
  | _::_::[] -> true
  | _::_::_::_::[] -> true
  | _ -> false

let lst_1_two_or_four = two_or_four_elements lst1
let lst_2_two_or_four = two_or_four_elements lst2
let lst_3_two_or_four = two_or_four_elements lst3;;

print_endline (string_of_bool (lst_1_two_or_four));
print_endline (string_of_bool (lst_2_two_or_four));
print_endline (string_of_bool (lst_3_two_or_four))
