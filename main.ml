(**
 * [compile code] is the transpiled JS code from the original OCaml [code].
 *
 * Raises: Failure if [code] is not a valid program and cannot be transpiled.
 *)
let compile code =
  Tokenizer.tokenize code
  |> Parser.parse
  |> Ast_converter.convert
  |> Optimizer.optimize
  |> Renderer.render

(**
 * [parse_args ()] is a tuple of an input file name and an output file name
 * if two inputs are provided. Otherwise it prints an error message and exits.
 *)
let parse_args () =
  try
    (Sys.argv.(1), Sys.argv.(2))
  with _ ->
    print_endline "Invalid arguments: camljs [input file] [output file]";
    exit 0

(**
 * This retrieves the user's input file and output file, reads the input OCaml
 * file, compiles it into JS code, and writes the result in the output file.
 * Exits if there are not two files provided.
 *
 * Raises: Failure if the input OCaml file is not valid and cannot be
 * transpiled.
 *)
let _ =
  let (input_file, output_file) = parse_args () in
  File_helper.read_file input_file
  |> compile
  |> File_helper.write_file output_file
