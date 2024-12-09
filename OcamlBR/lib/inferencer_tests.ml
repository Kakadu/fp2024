(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let%expect_test _ =
  let _ = Inferencer.Infer.infer_program_test {| let f x = x |} in
  [%expect {| val f : '0 -> '0 |}]
;;

let%expect_test _ =
  let _ = Inferencer.Infer.infer_program_test {| let f x = x + 2 |} in
  [%expect {| val f : int -> int |}]
;;

let%expect_test _ =
  let _ = Inferencer.Infer.infer_program_test {|let x = + 2 and n = "emf" |} in
  [%expect {|
    val n : string
    val x : int |}]
;;

let%expect_test _ =
  let _ =
    Inferencer.Infer.infer_program_test {|let x = 2 in let a = true in not a && x |}
  in
  [%expect {| Infer error: Unification failed on int and bool |}]
;;

let%expect_test _ =
  let _ = Inferencer.Infer.infer_program_test {| let a = if true then 2 + 9 else 1 |} in
  [%expect {| val a : int |}]
;;

let%expect_test _ =
  let _ = Inferencer.Infer.infer_program_test {| if a then 2 else 1 |} in
  [%expect {| Infer error: Undefined variable "a" |}]
;;
