(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MiniML.Parser
open MiniML.Inferencer

let test_inferencer s =
  match parse s with
  | Ok s ->
    (* print_endline (show_program s); *)
    (match inference s with
     | Ok env -> MiniML.Inferencer.print_env env
     | Error e -> Format.printf "Type inference error: %a" MiniML.Inferencer.pp_error e)
  | Error _ -> print_endline "Parsing error"
;;

let%expect_test "inference factorial function" =
  test_inferencer "let rec factorial n = if n < 2 then 1 else n * factorial(n - 1)";
  [%expect {|val factorial : (int -> int)|}]
;;

(* ========== const ========== *)

let%expect_test "inference int" =
  test_inferencer "let x = 2";
  [%expect {|val factorial : (int -> int)|}]
;;

let%expect_test "inference bool" =
  test_inferencer "let x = true";
  [%expect {|val factorial : (int -> int)|}]
;;

let%expect_test "inference unit" =
  test_inferencer "let x = ()";
  [%expect {|val factorial : (int -> int)|}]
;;

(* ========== bop ========== *)

let%expect_test "inference bop add sub mul div" =
  test_inferencer "let x = 23 + 23 - 45 - (2 * 345) / (-98)";
  [%expect {|
    46|}]
;;

(* ========== tuples ========== *)

let%expect_test "inference tuple fst" =
  test_inferencer "let f t = let (x, y) = t in x";
  [%expect {|
    23|}]
;;

let%expect_test "inference tuple 2" =
  test_inferencer "let (x, y) = (23, 12)";
  [%expect {|
    23|}]
;;

let%expect_test "inference tuple 3" =
  test_inferencer "let (x, y, z) = (23, 12, true)";
  [%expect {|
    23|}]
;;

(* ========== list ========== *)

let%expect_test "inference list pat" =
  test_inferencer "let [a] = [false]";
  [%expect {|
    1|}]
;;

(* ========== vars ========== *)

let%expect_test "inference var simple" =
  test_inferencer "let a = 23 let b = a let c = b";
  [%expect {|
    23|}]
;;

(* ========== fun ========== *)

let%expect_test "2+2" =
  test_inferencer
    {|
  let two = fun f -> fun x -> f (f x)
  let plus = fun m -> fun n -> fun f -> fun x -> m f (n f x) 
  let four = plus two two 
  let x = four (fun x -> x + 1) 0
  |};
  [%expect {|
    46|}]
;;

let%expect_test "2*2" =
  test_inferencer
    {|
  let two = fun f -> fun x -> f (f x)
  let mul = fun m -> fun n -> fun f -> fun x -> m (n f) x 
  let four = mul two two 
  let x = four (fun x -> x + 1) 0
  |};
  [%expect {|
    46|}]
;;
