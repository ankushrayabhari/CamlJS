let fifth_element lst = if (List.length lst) >= 5 then List.nth lst 4 else 0;;

print_int (fifth_element (2::3::[]@1::~-1::[1;2;3;4;5]))
