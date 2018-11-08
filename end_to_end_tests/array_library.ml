let lst1 = [|1;2;3;4;5|];;
let lst2 = [||];;
let lst3 = [|"asdf"; "qwer"|];;

print_endline (string_of_int (Array.get lst1 0));;
print_endline (string_of_int (Array.length lst2));;
print_endline (Array.get lst3 0);;

match lst3 with
| [|x1; x2;|] -> print_endline x2
| _ -> ()
