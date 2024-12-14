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
  run {| let f a b c = if a then b else c |};
  [%expect {|
    val f : bool -> 'c -> 'c -> 'c
  |}]
;;

let%expect_test "type check pattern matching" =
  run {| let f a b = match a b with 1 -> 'q' | 2 -> 'w' | _ -> 'e' |};
  [%expect {|
    val f : ('b -> int) -> 'b -> char
  |}]
;;

let%expect_test "type check expression constraint" =
  run {| let f a b = (b a : int) |};
  [%expect {|
    val f : 'a -> ('a -> int) -> int
  |}]
;;

let%expect_test "type check pattern constraint" =
  run {| let f (q : int -> char) (x : int) = q x |};
  [%expect {|
    val f : (int -> char) -> int -> char
  |}]
;;

let%expect_test "type check of expression list" =
  run {| let f a = [a; true] |};
  [%expect {|
    val f : bool -> bool list
  |}]
;;

let%expect_test "type check invalid expression list" =
  run {| let f a = [true; a; 2] |};
  [%expect {|
    Infer error: unification failed on bool and int
  |}]
;;

let%expect_test "type check of pattern list" =
  run {| let f a = match a with [q; q] -> q | [w; 2] -> w |};
  [%expect {|
    val f : int list -> int
  |}]
;;
