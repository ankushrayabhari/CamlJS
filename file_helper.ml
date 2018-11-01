open List

(**
 * [read_line_option in_ch] is either Some (s) where s is the line/string to EOF
 * at the beginning of [in_ch], or None if [in_ch] is at EOF.
 *)
let read_line_option in_ch =
  try
    Some (input_line in_ch)
  with
  | _ -> None

let read_file file =
  let in_ch =
    try open_in file with _ ->
      print_endline "Invalid input file.";
      exit 0
  in
  let rec read_file_lines curr_file =
    match read_line_option in_ch with
    | None -> rev curr_file
    | Some s -> read_file_lines (s::curr_file)
  in
  let lines = read_file_lines [] in
  close_in in_ch;
  lines |> String.concat "\n";;

let write_file file code =
  let out_ch =
    try open_out file with _ ->
      print_endline "Error opening output file.";
      exit 0
  in
  output_string out_ch code;
  output_string out_ch "\n"
