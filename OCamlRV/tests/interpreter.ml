(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Interpreter

(* Some arithmetic *)

let%expect_test _ =
  test_interpreter {|
      let a = ((2 + 2 - 2) * 7) / 2;;
      print_int a;;
   |};
  [%expect {| 7 |}]
;;

let%expect_test _ =
  test_interpreter
    {|
      let x = 1;;
      let y = x * 2;;
      let z = 3;;
      print_int (x + y + z);;
   |};
  [%expect {| 6 |}]
;;

(* Match, Function, Option, Tuple *)

let%expect_test _ =
  test_interpreter
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
  test_interpreter
    {|
      let a = function
      | Some x -> print_int x
      | None -> print_endline "None"
      ;;
      a (None);;
   |};
  [%expect {| None |}]
;;

let%expect_test _ =
  test_interpreter
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
  test_interpreter
    {|
      let a = [1; 2];;
      match a with
      | [0; 0] -> print_endline "Error"
      | [1; 2] ->  print_endline "Ok"
      ;;
   |};
  [%expect {| Ok |}]
;;

let%expect_test _ =
  test_interpreter {|
      let () = print_int 1;;
   |};
  [%expect {| 1 |}]
;;

let%expect_test _ =
  test_interpreter {|
      let _ = print_int 1;;
   |};
  [%expect {| 1 |}]
;;

let%expect_test _ =
  test_interpreter
    {|
      let (a, b, _) = (1, 2, 3);;
      print_int a;;
      print_int b;;
   |};
  [%expect {| 12 |}]
;;

let%expect_test _ =
  test_interpreter
    {|
      let () = 
        let (a, b, c) = (1, 2, 3) in 
        print_int (a + b + c);;
   |};
  [%expect {| 6 |}]
;;

let%expect_test _ =
  test_interpreter
    {|
      let a = 1 and b = 2 and c = 3;;
      print_int a;;
      print_int b;;
      print_int c;;
   |};
  [%expect {| 123 |}]
;;

let%expect_test _ =
  test_interpreter
    {|
      let x =
        let a = 1
        and b = 2
        and c = 3 in
        a + b + c
      ;;
      print_int x;;
   |};
  [%expect {| 6 |}]
;;

let%expect_test _ =
  test_interpreter
    {|
      let Some x = Some 1;;
      let a, Some y = 1, Some 2;;
      print_int x;;
      print_int y;;
   |};
  [%expect {| 12 |}]
;;

(* Mutual recursion *)

let%expect_test _ =
  test_interpreter
    {|
    let rec even n =
        match n with
          | 0 -> 1
          | x -> odd (x-1)
    and odd n =
        match n with
          | 0 -> 0
          | x -> even (x-1);;
    print_int (odd 10);;
    print_int (even 10);;
   |};
  [%expect {| 01 |}]
;;

let%expect_test _ =
  test_interpreter
    {|
      let () =
        let rec even n =
          match n with
            | 0 -> 1
            | x -> odd (x-1)
        and odd n =
          match n with
            | 0 -> 0
            | x -> even (x-1) in
        print_int (even 7)
   |};
  [%expect {| 0 |}]
;;

(* Some functions *)

let%expect_test _ =
  test_interpreter
    {|
      let rec f n = if n <= 1 then 1 else n * f (n - 1);;
      print_int (f 5);;
   |};
  [%expect {| 120 |}]
;;

let%expect_test _ =
  test_interpreter
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
