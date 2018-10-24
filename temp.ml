let tokenized_program =
  Tokenizer.tokenize "nacci (n - 1)" |> Array.of_list in
let parse_tree = Parser.parse tokenized_program in
()
