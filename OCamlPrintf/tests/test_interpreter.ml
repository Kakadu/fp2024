(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Parser
open Ocaml_printf_lib.Interpreter

let run str =
  match parse str with
  | Ok ast ->
    (match run_interpreter empty_env ast with
     | Ok (_, out_list) ->
       List.iter
         (function
           | Some id, val' -> Format.printf "val %s = %a\n" id pp_value val'
           | None, val' -> Format.printf "- = %a\n" pp_value val')
         out_list
     | Error e -> Format.printf "Interpreter error: %a\n" pp_error e)
  | Error _ -> Format.printf "Parsing error\n"
;;

let%expect_test "parsing error" =
  run {|
  let a = ;;
  |};
  [%expect {|
  Parsing error
  |}]
;;

let%expect_test "eval simple let binding" =
  run {|
  let a = -(4 + 4)
  and b = true;;
  |};
  [%expect {|
  val a = -8
  val b = true
  |}]
;;

let%expect_test "eval tuple and list let bindings" =
  run {|
  let a, b = 1, (2, 3);;
  let [ c; d ] = 3 :: 4 :: []
  |};
  [%expect {|
  val a = 1
  val b = (2, 3)
  val c = 3
  val d = 4
  |}]
;;

let%expect_test "eval `let in'" =
  run {|
  let f =
    let x = "abc" in
    let y = "qwerty" in
    x <> y
  ;;
  |};
  [%expect {|
  val f = true
  |}]
;;

let%expect_test "eval 'Struct_eval'" =
  run {|
  1;;
  |};
  [%expect {|
  - = 1
  |}]
;;

let%expect_test "eval 'Exp_fun'" =
  run {|
  let foo x y = x * y
  let q = foo 1 6
  let w = foo 2 (-5)
  |};
  [%expect {|
  val foo = <fun>
  val q = 6
  val w = -10
  |}]
;;

let%expect_test "eval recursive value binding 1" =
  run {|
  let rec x = 21 and y = x + 1;;
  |};
  [%expect {|
  val x = 21
  val y = 22
  |}]
;;

let%expect_test "eval recursive value binding 2" =
  run
    {|
  let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1);;
  factorial 5
  |};
  [%expect {|
  val factorial = <fun>
  - = 120
  |}]
;;

let%expect_test "eval pattern-matching" =
  run
    {|
  let f =
    match [ 1; 2; 3 ] with
    | a :: [] -> a
    | a :: b :: [] -> a + b
    | a :: b :: c :: [] -> a + b + c
    | _ -> 0
  ;;
  |};
  [%expect {|
  val f = 6
  |}]
;;
