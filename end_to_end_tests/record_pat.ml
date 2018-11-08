type lol = {
  laugh: string;
  temp: int * (bool * char)
}

let x = {
  laugh = "asdf";
  temp = 1, (true, 'a')
};;

match x with
  {laugh=x; temp=_;} -> print_endline x;;
