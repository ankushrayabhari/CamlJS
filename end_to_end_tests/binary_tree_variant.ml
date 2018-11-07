type binary_tree =
  | Leaf
  | Node of binary_tree * int * binary_tree

let rec in_order_print tr = match tr with
  | Node (l, v, r) ->
      in_order_print l;
      print_endline (string_of_int v);
      in_order_print r
  | Leaf -> ()

let massive_tree = Node (
    Node (
      Node (Leaf, 1, Leaf),
      2,
      Node (Leaf, 3, Leaf)
    ),
    9,
    Node (
      Node (Leaf, 5, Leaf),
      6,
      Node (Leaf, 7, Leaf)
    )
);;

in_order_print massive_tree;;
