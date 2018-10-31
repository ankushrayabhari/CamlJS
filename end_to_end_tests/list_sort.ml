let sort_list_descending' lst = List.rev (List.sort Pervasives.compare lst);;
let initial_list = [1;5;2;8;9];;
let final_list = sort_list_descending' initial_list;;
List.iter Pervasives.print_int final_list;print_endline ""
