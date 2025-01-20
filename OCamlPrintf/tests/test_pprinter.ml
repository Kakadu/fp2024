(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Test_parser

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
