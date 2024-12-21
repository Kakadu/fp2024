(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Inferencer.Infer

(*
let infer_from_file file_name =
  let file_path = "../tests/inferencer_tests/" ^ file_name in
  let input = 
    let ic = open_in file_path in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    content
  in
  let result = infer_program_test input in
  result

let%expect_test "do_not_type_001" =
  let _ = infer_from_file "do_not_type/001.ml" in
  [%expect {| val ..3232. |}]
(*
let%expect_test "do_not_type_002if" =
  let _ = infer_from_file "do_not_type/002if.ml" in
  [%expect {| val ... |}]
*)


*)


let%expect_test _ =
  let _ = infer_program_test {|let n x = x in let f g = g 3 in f n  |} in
  [%expect {| |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let f x = x |} in
  [%expect {| val f : '0 -> '0 |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let f x = x + 2 |} in
  [%expect {| val f : int -> int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let x = 2 in x=1 |} in
  [%expect {|  |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let fac n = if n < 1 then 1 else n * fac (n - 1) |} in
  [%expect {| val fac : int -> int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let rec x = 1+ x|} in
  [%expect {| val x : int |}]
;;

let%expect_test _ =
  let _ =
    infer_program_test
      {|let rec is_even n = if n = 0 then true else is_odd (n - 1) and is_odd n = if n = 0 then false else is_even (n - 1)|}
  in
  [%expect {|
    val is_even : int -> bool
    val is_odd : int -> bool |}]
;;

let%expect_test _ =
  let _ =
    infer_program_test {|let square x = x*x in let id x = x in (id square) (id  2) |}
  in
  [%expect {| |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let x = 2 in let a = true in not a && x |} in
  [%expect {| Infer error: Unification failed on int and bool |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let rec f x = f |} in
  [%expect {| Infer error: Occurs check failed |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let a = if true then 2 + 9 else 1 |} in
  [%expect {| val a : int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| if a then 2 else 1 |} in
  [%expect {| Infer error: Undefined variable "a" |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| fun x y -> x + y |} in
  [%expect {|  |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let f x y z w = if y&&z then x else  w + 1 |} in
  [%expect {| val f : int -> (bool -> (bool -> (int -> int))) |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let a = fun x::y::z::w -> if z > 0 then y else x |} in
  [%expect {| val a : int list -> int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let b = fun (a,b,(2::t), d) -> a + d  |} in
  [%expect {| val b : (int * '1 * int list * int) -> int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let (<|>) a b = a/b + b*a |} in
  [%expect {| val <|> : int -> (int -> int) |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let w [2; v] (y, dx, d) = (-4, 5+v, true&&d) |} in
  [%expect {| val w : int list -> (('2 * '3 * bool) -> (int * int * bool)) |}]
;;

let%expect_test _ =
  let _ =
    infer_program_test {|let f = fun ((3, true): int*bool) x -> if x then 4 else 0  |}
  in
  [%expect {| val f : (int * bool) -> (bool -> int) |}]
;;
