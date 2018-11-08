let rec sum n acc =
  if n <= 0 then acc else
  (sum (n - 1) (acc + n));;

print_int (sum 1000 0)
