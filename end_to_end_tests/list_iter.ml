open List

let 1::x = [1;2;3;4;];;

iter (fun el -> print_endline (string_of_int el)) x
