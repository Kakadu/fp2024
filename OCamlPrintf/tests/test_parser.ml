(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Parser
open Ocaml_printf_lib.Pprinter

let run str =
  match parse str with
  | Ok ast -> Format.printf "%a \n" pp_structure ast
  | Error error -> Format.printf "%s" error
;;

let%expect_test "parsing value structure and factorial with `match'" =
  run
    {|
  let rec factorial n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | _ -> n * factorial (n - 1)
  ;;
  |};
  [%expect
    {|
  let rec factorial = (fun n -> (match n with | 0 -> 1 | 1 -> 1 | _ -> n * (factorial (n - 1))));;
  |}]
;;

let%expect_test "parsing expression with `fun'" =
  run {|
  let sum = fun x -> (fun y -> x + y);;
  |};
  [%expect {|
  let sum = (fun x -> (fun y -> x + y));;
  |}]
;;

let%expect_test "parsing pattern and expression tuples" =
  run {|
  let a, b = 1, 2
  |};
  [%expect {|
  let (a, b) = (1, 2);;
  |}]
;;

let%expect_test "parsing expression list" =
  run {|
  let list_ = [1; 2; 3]
  |};
  [%expect {|
  let list_ = [1; 2; 3];;
  |}]
;;

let%expect_test "parsing option and bool types" =
  run {|
  let f a =
    match a with
    | Some _ -> true
    | None -> false
  ;;
  |};
  [%expect
    {|
  let f = (fun a -> (match a with | Some (_) -> true | None -> false));;
  |}]
;;

let%expect_test "parsing chain right associative" =
  run {|
  let f x y z = if x = 0 && y = 1 || z >= 2 then 2 else 26;;
  |};
  [%expect
    {|
  let f = (fun x y z -> (if x = 0 && y = 1 || z >= 2 then 2 else 26));;
  |}]
;;

let%expect_test "parsing evaluation structure and chain left associative" =
  run {|
  8 / 800 - 555 * (35 + 35)
  |};
  [%expect {|
  8 / 800 - 555 * (35 + 35);;
  |}]
;;

let%expect_test "parsing expression with `let'" =
  run {|
  1 + let two = 2 in two * 3
  |};
  [%expect {|
  1 + (let two = 2 in two * 3);;
  |}]
;;

let%expect_test "parsing several structure items" =
  run {|
  let squared x = x * x;; squared 5
  |};
  [%expect {|
  let squared = (fun x -> x * x);;
  (squared 5);;
  |}]
;;

let%expect_test "parsing sequence and exepression construct" =
  run {|
  [1; 2; 3]; "qwerty123"
  |};
  [%expect {|
  ([1; 2; 3]); ("qwerty123");;
  |}]
;;

let%expect_test "parsing identifiers with explicitly assigned types 1" =
  run {|
  let f : int list = [1; 2; 3];;
  |};
  [%expect {|
  let f : int list = [1; 2; 3];;
  |}]
;;

let%expect_test "parsing identifiers with explicitly assigned types 2" =
  run
    {|
  let f : int * char * string list = (1, 'a', ["first"; "second"; "third"]);;
  |};
  [%expect
    {|
  let f : (int * char * string list) = (1, 'a', ["first"; "second"; "third"]);;
  |}]
;;

let%expect_test "parsing identifiers with explicitly assigned types 3" =
  run {|
  let f (a : int) (b : int) : int = a + b;;
  |};
  [%expect {|
  let f = (fun (a : int) (b : int) : int -> a + b);;
  |}]
;;

let%expect_test "parsing identifiers with explicitly assigned types 4" =
  run {|
  let (a : int -> (char -> int) -> int) = 1 + (x : char -> int);;
  |};
  [%expect {|
  let (a : int -> (char -> int) -> int) = 1 + (x : char -> int);;
  |}]
;;
