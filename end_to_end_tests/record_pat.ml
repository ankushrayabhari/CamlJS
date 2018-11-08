type lol = {
  laugh: string;
  temp: int
}

let x = {
  laugh = "asdf";
  temp = 1
};;

match x with
  {laugh=x; temp=_;} -> print_endline x;;

print_endline (string_of_int (x.temp))
