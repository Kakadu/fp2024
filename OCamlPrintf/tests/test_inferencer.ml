(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Parser
open Ocaml_printf_lib.Pprinter
open Ocaml_printf_lib.Inferencer

let run str =
  match parse str with
  | Ok ast ->
    (match run_inferencer ast with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(Scheme (_, ty)) ->
         Format.printf "val %s : %a\n" key pp_core_type ty)
     | Error e -> Format.printf "Infer error: %a\n" pp_error e)
  | Error _ -> Format.printf "Parsing error\n"
;;

let%expect_test "type check undefined variable" =
  run {| b |};
  [%expect {| Infer error: Undefined variable 'b' |}]
;;

let%expect_test "type check definition variable" =
  run {| let a = 5 |};
  [%expect {| val a : int |}]
;;

let%expect_test "type check several definition variable" =
  run {| let f = 1 and r = "qwe";; let q = 2 |};
  [%expect {|
    val f : int
    val q : int
    val r : string
  |}]
;;

let%expect_test "type check definition function" =
  run {| let f a = 1 |};
  [%expect {|
    val f : 'a -> int
  |}]
;;
