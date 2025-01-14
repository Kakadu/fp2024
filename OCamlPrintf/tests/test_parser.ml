(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Parser
open Ocaml_printf_lib.Pprinter

let run str =
  match parse str with
  | Ok ast -> Format.printf "%a \n" pp_structure ast
  | Error error -> Format.printf "%s" error
;;

let%expect_test "parsing error" =
  run {|
  let a = ;;
  |};
  [%expect {|
  : end_of_input
  |}]
;;

let%expect_test "parsing factorial with `match'" =
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
  let rec factorial n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | _ -> n * factorial (n - 1)
  ;;
  |}]
;;

let%expect_test "parsing expression with `fun'" =
  run {|
  let sum1 = fun x y -> (x + y)
  let sum2 = fun x -> (fun y -> x + y)
  |};
  [%expect {|
  let sum1 x y = x + y;;
  let sum2 x = fun y -> x + y;;
  |}]
;;

let%expect_test "parsing pattern and expression tuples" =
  run {|
  let a, b = 1, 2
  let a, b, c = -1, 2 + 3, f d
  |};
  [%expect {|
  let a, b = 1, 2;;
  let a, b, c = -1, 2 + 3, f d;;
  |}]
;;

let%expect_test "parsing pattern and expression list" =
  run
    {|
  let list [ a; b; c ] = [ a; b; c ];;
  let foo1 = f [ a; b ];;
  let foo2 = [ f a; f b ];;
  let foo3 = f [ f a; f b ];;
  let foo4 = f [ f a; [ f a; f b ] ];;
  let foo5 = f [ [ f a; [ f a; f b ] ] ];;
  [] + [];;
  [ 1 + 2; -3; f a ] + [ f a; f b ];;
  [ [ [] + []; -3; f a ] ] + [ [ f a; f b ] ]
  |};
  [%expect
    {|
  let list [ a; b; c ] = [ a; b; c ];;
  let foo1 = f [ a; b ];;
  let foo2 = [ f a; f b ];;
  let foo3 = f [ f a; f b ];;
  let foo4 = f [ f a; [ f a; f b ] ];;
  let foo5 = f [ [ f a; [ f a; f b ] ] ];;
  [] + [];;
  [ 1 + 2; -3; f a ] + [ f a; f b ];;
  [ [ [] + []; -3; f a ] ] + [ [ f a; f b ] ];;
  |}]
;;

let%expect_test "parsing option and bool types" =
  run
    {|
  let f = function
    | Some (_) -> true
    | None -> false
  ;;
  Some true;;
  Some (Some true)
  |};
  [%expect
    {|
  let f = function
          | Some (_) -> true
          | None -> false;;
  Some (true);;
  Some (Some (true));;
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
  let squared x = x * x;;
  squared 5
  |};
  [%expect {|
  let squared x = x * x;;
  squared 5;;
  |}]
;;

let%expect_test "parsing expression sequence" =
  run
    {|
  let a = (1, 2, ((); 3));;
  [ (a; b) ];;
  [ f a; [ () ]; ((); []) ];;
  let a = [ ( (); 1); ( ( (); 2)); ( ((); (); 3) ); (((); 4); 5)]
  |};
  [%expect
    {|
  let a = 1, 2, ((); 3);;
  [ (a; b) ];;
  [ f a; [ () ]; ((); []) ];;
  let a = [ ((); 1); ((); 2); (((); ()); 3); (((); 4); 5) ];;
  |}]
;;

let%expect_test "parsing identifiers with explicitly assigned types 1" =
  run {|
  let f : int list = [ 1; 2; 3 ];;
  |};
  [%expect {|
  let f : int list = [ 1; 2; 3 ];;
  |}]
;;

let%expect_test "parsing identifiers with explicitly assigned types 2" =
  run
    {|
  let f : int * char * string list = (1, 'a', ["first"; "second"; "third"]);;
  |};
  [%expect
    {|
  let f : int * char * string list = 1, 'a', [ "first"; "second"; "third" ];;
  |}]
;;

let%expect_test "parsing identifiers with explicitly assigned types 3" =
  run {|
  let f (a : int) (b : int) : int = a + b;;
  |};
  [%expect {|
  let f (a : int) (b : int) : int = a + b;;
  |}]
;;

let%expect_test "parsing identifiers with explicitly assigned types 4" =
  run {|
  let (a : int -> (char -> int) -> int) = 1 + (x : char -> int);;
  |};
  [%expect {|
  let a : int -> (char -> int) -> int = 1 + (x : char -> int);;
  |}]
;;

let%expect_test "parsing chain right associative" =
  run
    {|
  let f x y z = if x && (y || z && (y || x) || y) then true else false;;
  let list (a :: b :: [ c ]) = a :: b :: [ c + 1 ]
  |};
  [%expect
    {|
  let f x y z = if x && (y || z && (y || x) || y) then true else false;;
  let list [ a; b; c ] = [ a; b; c + 1 ];;
  |}]
;;

let%expect_test "parsing chain left associative" =
  run
    {|
  8 / 800 - 555 * (35 + 35);;
  let f x y z = if x = (y >= z && (y <= x) = y) then true else false;;
  let f a b c = g a (b + c) b (a * b);;
  let f a b c = a; b a; c [ a ];;
  let f a : (int option list * unit option -> bool list option list) * string option option = a
  |};
  [%expect
    {|
  8 / 800 - 555 * (35 + 35);;
  let f x y z = if x = (y >= z && y <= x = y) then true else false;;
  let f a b c = g a (b + c) b (a * b);;
  let f a b c = (a; b a); c [ a ];;
  let f
      a
      : (int option list * unit option -> bool list option list) * string option option
      = a
  ;;
  |}]
;;

let%expect_test "parsing expression with priority" =
  run
    {|
  1 + 2 + 3;;
  (1 + 2) - 3;;
  (1 + 2) * 3;;
  3 * (1 + 2);;
  (1 + 2) * (3 + 4);;
  1 * 2 * (3 + 4);;
  (1 + 2) * 3 * 4;;
  1 / 2 - 3 * 4;;
  g * f a (b + c) (d e)
  |};
  [%expect
    {|
  1 + 2 + 3;;
  1 + 2 - 3;;
  (1 + 2) * 3;;
  3 * (1 + 2);;
  (1 + 2) * (3 + 4);;
  1 * 2 * (3 + 4);;
  (1 + 2) * 3 * 4;;
  1 / 2 - 3 * 4;;
  g * f a (b + c) (d e);;
  |}]
;;

let%expect_test "parsing negative expressions" =
  run
    {|
  -2 + 1;;
  -(2 + -2);;
  -(-1 + 1);;
  let f a = -a;;
  let f a = -(if a then -1 else 2);;
  g * f (-a) (-b + c) (d (-e))
  |};
  [%expect
    {|
  -2 + 1;;
  -(2 + -2);;
  -(-1 + 1);;
  let f a = -a;;
  let f a = -(if a then -1 else 2);;
  g * f (-a) (-b + c) (d (-e));;
  |}]
;;
