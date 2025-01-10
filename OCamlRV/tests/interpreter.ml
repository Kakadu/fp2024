(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Interpreter

(* Some arithmetic *)

let%expect_test _ =
  test_interpret {|
      let a = ((2 + 2 - 2) * 7) / 2;;
      print_int a;;
   |};
  [%expect {| 7 |}]
;;

let%expect_test _ =
  test_interpret
    {|
      let x = 1;;
      let y = x * 2;;
      let z = 3;;
      print_int (x + y + z);;
   |};
  [%expect {| 6 |}]
;;

(* Match, Option, Tuple *)

let%expect_test _ =
  test_interpret
    {|
      let a = Some 123;;
      match a with
      | Some x -> print_int x
      | None -> print_endline "None"
      ;;
   |};
  [%expect {| 123 |}]
;;

let%expect_test _ =
  test_interpret
    {|
      let a = (1, 2);;
      match a with
      | (0, 0) -> print_endline "Error"
      | (1, 2) ->  print_endline "Ok"
      ;;
   |};
  [%expect {| Ok |}]
;;

let%expect_test _ =
  test_interpret
    {|
      let a = [1; 2];;
      match a with
      | [0; 0] -> print_endline "Error"
      | [1; 2] ->  print_endline "Ok"
      ;;
   |};
  [%expect {| Ok |}]
;;

(* Some functions *)

let%expect_test _ =
  test_interpret
    {|
      let rec f n = if n <= 1 then 1 else n * f (n - 1);;
      print_int (f 5);;
   |};
  [%expect {| 120 |}]
;;

let%expect_test _ =
  test_interpret
    {|
      let rec sum_list lst =
        match lst with
        | [] -> 0
        | head :: tail -> head + sum_list tail
      ;;
      print_int (sum_list [1; 2; 3; 4; 5]);;
   |};
  [%expect {| 15 |}]
;;
