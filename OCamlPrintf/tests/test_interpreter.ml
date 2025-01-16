(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Parser
open Ocaml_printf_lib.Interpreter

let run str =
  match parse str with
  | Ok ast ->
    (match Inter.eval_structure ast with
     | Ok (env, val_list) ->
       Base.Map.iteri env ~f:(fun ~key ~data ->
         Format.printf "val %s = %a\n" key pp_value data);
       List.iter (fun val_exp -> Format.printf "- = %a\n" pp_value val_exp) val_list
     | Error e -> Format.printf "Interpreter error: %a\n" pp_error e)
  | Error _ -> Format.printf "Parsing error\n"
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
