type x_t =
  | X of int
  | DoubleX of int * int
  | ListX of int list

let c l =
  match l with
  | x :: [] -> X x
  | [ x; y ] -> DoubleX (x, y)
  | _ -> ListX l
;;

match X 1 with
| X x -> print_int x
| DoubleX (x, y) ->
  print_int x;
  prerr_int y
| ListX l -> print_endline "list"
