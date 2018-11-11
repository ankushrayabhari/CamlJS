open Printf

let end_to_end_test_dir = "end_to_end_tests";;
let files = Sys.readdir end_to_end_test_dir;;

Array.iter (fun file ->
  Sys.command "echo \"\" > e2e-compiled.js" |> ignore;
  Sys.command "echo \"\" > e2e-js-result.txt" |> ignore;
  Sys.command "echo \"\" > e2e-ml-result.txt" |> ignore;
  Sys.command "echo \"\" > e2e-diff.txt" |> ignore;
  let full_file = sprintf "%s/%s" end_to_end_test_dir file in
  let compile = sprintf "./camljs %s e2e-compiled.js" full_file in
  let node = "node e2e-compiled.js &> e2e-js-result.txt" in
  let ocaml = sprintf "ocaml -w \"-A\" %s &> e2e-ml-result.txt" full_file in
  let diff = "diff e2e-ml-result.txt e2e-js-result.txt > e2e-diff.txt" in
  let _ = Sys.command compile in
  let _ = Sys.command node in
  let _ = Sys.command ocaml in
  let _ = Sys.command diff in
  match File_helper.read_file "e2e-diff.txt" with
  | "" -> print_endline ("Passed: " ^ full_file)
  | str ->
    print_endline ("Failed: " ^ full_file);
    print_endline str;
    print_endline ""
) files;
Sys.remove "e2e-compiled.js";
Sys.remove "e2e-js-result.txt";
Sys.remove "e2e-ml-result.txt";
Sys.remove "e2e-diff.txt";
