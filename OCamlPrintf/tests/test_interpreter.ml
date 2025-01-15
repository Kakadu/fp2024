(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Parser
open Ocaml_printf_lib.Pprinter
open Ocaml_printf_lib.Interpreter

let run str =
  match parse str with
  | Ok ast ->
    (match Inter.eval_structure ast with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data ->
         Format.printf "%s : %a\n" key pp_value data)
     | Error e -> Format.printf "Interpreter error: %a\n" pp_error e)
  | Error _ -> Format.printf "Parsing error\n"
;;

let%expect_test "eval simple let binding" =
  run {|
  let a = -(4 + 4)
  and b = true;;
  |};
  [%expect {|
  a : -8
  b : true
  |}]
;;

let%expect_test "eval tuple let binding" =
  run {|
  let a, b = 1, (2, 3)
  |};
  [%expect {|
  a : 1
  b : (2, 3)
  |}]
;;

let%expect_test "eval let in" =
  run {|
  let f =
    let x = "abc" in
    let y = "qwerty" in
    x <> y
  ;;
  |};
  [%expect {|
  f : true
  |}]
;;
