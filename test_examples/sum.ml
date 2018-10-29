let rec sum n =
  if n <= 0 then 0 else
  (sum (n - 1)) + n
in print_int (sum 5)
