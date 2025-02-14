(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Inferencer.Infer
open Typedtree

let infer_program_test s =
  let open Stdlib.Format in
  let open Interpreter in
  match Parser.parse_expr s with
  | Ok parsed ->
    (match infer_program parsed with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(S (_, ty)) ->
         if print_key key then printf "val %s : %a\n" key pp_ty ty)
     | Error e -> printf "Infer error: %a\n" pp_error e)
  | Error e -> printf "Parsing error: %s\n" e
;;

let%expect_test "infer id function" =
  let _ = infer_program_test {| let f x = x |} in
  [%expect {| val f : '0 -> '0 |}]
;;

let%expect_test "infer simple function with int args" =
  let _ = infer_program_test {| let f x = x + 2 |} in
  [%expect {| val f : int -> int|}]
;;

let%expect_test "infer seval (no value)" =
  let _ = infer_program_test {|let x = 2 in x = 1 |} in
  [%expect {|  |}]
;;

let%expect_test "infer factorial function" =
  let _ = infer_program_test {|let rec fac n = if n < 1 then 1 else n * fac (n - 1) |} in
  [%expect {| val fac : int -> int |}]
;;

let%expect_test "infer mutual recursion" =
  let _ =
    infer_program_test
      {|let rec is_even n = if n = 0 then true else is_odd (n - 1) and is_odd n = if n = 0 then false else is_even (n - 1)|}
  in
  [%expect {|
    val is_even : int -> bool
    val is_odd : int -> bool |}]
;;

let%expect_test "infer function with polymorphism" =
  let _ =
    infer_program_test {|let square x = x*x in let id x = x in (id square) (id  2) |}
  in
  [%expect {| |}]
;;

let%expect_test "infer function that can't be unified" =
  let _ = infer_program_test {|let x = 2 in let a = true in not a && x |} in
  [%expect {| Infer error: Unification failed on int and bool |}]
;;

let%expect_test "infer function with occures check" =
  let _ = infer_program_test {| let rec f x = f |} in
  [%expect {| Infer error: Occurs check failed: type variable 0 inside type '1 -> '0 |}]
;;

let%expect_test "infer svalue with if-then-else" =
  let _ = infer_program_test {| let a = if true then 2 + 9 else 1 |} in
  [%expect {| val a : int |}]
;;

let%expect_test "infer undefined variable" =
  let _ = infer_program_test {| if a then 2 else 1 |} in
  [%expect {| Infer error: Undefined variable "a" |}]
;;

let%expect_test "infer function with 2 args" =
  let _ = infer_program_test {| let a = fun x y -> x + y |} in
  [%expect {| val a : int -> (int -> int) |}]
;;

let%expect_test "infer function with many args" =
  let _ = infer_program_test {| let f x y z w = if y&&z then x else  w + 1 |} in
  [%expect {| val f : int -> (bool -> (bool -> (int -> int))) |}]
;;

let%expect_test "infer function with non-trivial arg" =
  let _ = infer_program_test {| let a = fun x::y::z::w -> if z > 0 then y else x |} in
  [%expect {| val a : int list -> int |}]
;;

let%expect_test "infer function with another non-trivial arg" =
  let _ = infer_program_test {| let b = fun (a,b,(2::t), d) -> a + d  |} in
  [%expect {| val b : (int * '1 * int list * int) -> int |}]
;;

let%expect_test "infer prefix operator" =
  let _ = infer_program_test {| let (<|>) a b = a/b + b*a |} in
  [%expect {| val <|> : int -> (int -> int) |}]
;;

let%expect_test "infer function with list and tuple args" =
  let _ = infer_program_test {|let w [2; v] (y, dx, d) = (-4, 5+v, true&&d) |} in
  [%expect {| val w : int list -> (('2 * '3 * bool) -> (int * int * bool)) |}]
;;

let%expect_test "infer function with ascription" =
  let _ =
    infer_program_test {|let f = fun ((3, true): int*bool) x -> if x then 4 else 0  |}
  in
  [%expect {| val f : (int * bool) -> (bool -> int) |}]
;;

let%expect_test "infer expr with unary and binary operations" =
  let _ =
    infer_program_test
      {| 
    let rez = 
      let x = not true in 
      let y = 13 in if x || (10 >= y) && (5 <= y) && (y <> 6) || (y < 9) && (y > -1000) then +5 :: [] else [10] |}
  in
  [%expect {| val rez : int list |}]
;;

let%expect_test "infer expr with multiple patterns" =
  let _ =
    infer_program_test
      {| 
    let a : string = "fef"
    let b : (string * int list * bool) = ("a", [4], not true) |}
  in
  [%expect {|
    val a : string
    val b : (string * int list * bool) |}]
;;
