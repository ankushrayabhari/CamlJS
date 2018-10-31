let compile code =
  Tokenizer.tokenize code
  |> Array.of_list
  |> Parser.parse
  |> Ast_converter.convert
  |> Renderer.render

let parse_args () =
  try
    (Sys.argv.(1), Sys.argv.(2))
  with _ ->
    print_endline "Invalid arguments: camljs [input file] [output file]";
    exit 0

let _ =
  let (input_file, output_file) = parse_args () in
  File_helper.read_file input_file
  |> compile
  |> File_helper.write_file output_file
