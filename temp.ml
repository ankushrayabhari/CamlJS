let rec nacci n =
  if n = 1 then 1
  else if n = 2 then 1
  else nacci (n - 1) + nacci (n - 2)
in nacci 3
