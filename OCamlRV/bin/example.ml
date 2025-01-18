let rec list_iter l n =
  if n = 0
  then ()
  else (
    match l with
    | [] -> ()
    | [ x ] -> print_int x
    | x :: xs ->
      let () = print_int x in
      list_iter xs (n - 1))
;;

list_iter [ 1; 2; 3; 4; 5 ] 5
