let tokenized_program =
  Tokenizer.tokenize "10 * x - 300 + 1000" |> Array.of_list in
let parse_tree = Parser.parse tokenized_program in
()
