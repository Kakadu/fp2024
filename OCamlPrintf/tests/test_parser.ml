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
  let sum = fun x -> (fun y -> x + y);;
  |};
  [%expect {|
  let sum x = fun y -> x + y;;
  |}]
;;

let%expect_test "parsing pattern and expression tuples" =
  run {|
  let a, b = 1, 2
  |};
  [%expect {|
  let a, b = 1, 2;;
  |}]
;;

let%expect_test "parsing expression list" =
  run {|
  let list_ = [ 1; 2; 3 ]
  |};
  [%expect {|
  let list_ = [ 1; 2; 3 ];;
  |}]
;;

let%expect_test "parsing pattern and expression list construct" =
  run
    {|
  let list = 1 :: 2 :: [3] in
  match list with
  | 1 :: 2 :: [3] -> true
  | _ -> false
  |};
  [%expect
    {|
  let list = [ 1; 2; 3 ] in
  (match list with
   | [ 1; 2; 3 ] -> true
   | _ -> false);;
  |}]
;;

let%expect_test "parsing option and bool types" =
  run {|
  let f = function
    | Some (_) -> true
    | None -> false
  ;;
  |};
  [%expect
    {|
  let f = function
          | Some (_) -> true
          | None -> false;;
  |}]
;;

let%expect_test "parsing chain right associative" =
  run {|
  let f x y z = if x = 0 && y = 1 || z >= 2 then 2 else 26;;
  |};
  [%expect {|
  let f x y z = if x = 0 && y = 1 || z >= 2 then 2 else 26;;
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
  let squared x = x * x;;
  squared 5;;
  |}]
;;

let%expect_test "parsing sequence and exepression construct" =
  run {|
  [ 1; 2; 3 ]; "qwerty123"
  |};
  [%expect {|
  [ 1; 2; 3 ]; "qwerty123";;
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
  ;;
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
  |};
  [%expect
    {|
  -2 + 1;;
  -(2 + -2);;
  -(-1 + 1);;
  let f a = -a;;
  let f a = -(if a then -1 else 2);;
  |}]
;;

let%expect_test "parsing and pretty printing" =
  run
    {|
if true then 1 else 0;;

let a b = if true then 1 else 0;;

match
  function
  | _ -> true
with
| b -> true
;;

if match b with
   | b -> true
then (
  match b with
  | b -> true)
else (
  match b with
  | b -> true)
;;

let a b =
  match b with
  | b ->
    (match
       function
       | _ -> true
     with
     | b -> true)
;;

let a b =
  if match b with
     | b -> true
  then (
    match b with
    | b -> true)
  else (
    match b with
    | b -> true)
;;

let f a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a = 1;;

match b with
| [ a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a ] -> true
| [ a; a; a; a; a ] -> false
| a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a -> true
| a, a, a, a, a, a -> false
| _ -> false
;;
  |};
  [%expect
    {|
if true then 1 else 0;;
let a b = if true then 1 else 0;;
match
  function
  | _ -> true with
| b -> true;;
if match b with
   | b -> true
then
  (match b with
   | b -> true)
else
  (match b with
   | b -> true);;
let a b = match b with
          | b ->
            (match
              function
              | _ -> true with
             | b -> true);;
let a b =
  if match b with
     | b -> true
  then
    (match b with
     | b -> true)
  else
    (match b with
     | b -> true)
;;
let f
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    a
    = 1
;;
match b with
| [ a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ; a
  ] -> true
| [ a; a; a; a; a ] -> false
| ( a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a
  , a ) -> true
| ( a, a, a, a, a, a ) -> false
| _ -> false;;
  |}]
;;
