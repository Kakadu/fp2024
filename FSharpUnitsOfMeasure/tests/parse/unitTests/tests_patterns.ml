(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Pprint.Pp
open Parse.Patterns
open Pprint.Pprinter

let run = pp pprint_pat ppat 

(************************** Idents **************************)

let%expect_test "single underscore should be parsed as a wildcard" =
  run {| _ |};
  [%expect {| _ |}]
;;

let%expect_test "parse ident as pattern ident" =
  run {| x |};
  [%expect {| x |}]
;;

let%expect_test "parse + inside parentheses as pattern ident" =
  run {| (+) |};
  [%expect {| + |}]
;;

let%expect_test "parse +. operator inside parentheses as pattern ident" =
  run {| (+.) |};
  [%expect {| +. |}]
;;

(************************** Consts **************************)

let%expect_test "parse const int as pattern const int" =
  run {| 1 |};
  [%expect {| 1 |}]
;;

(************************** Tuples **************************)

let%expect_test "parse pattern tuple with 0 elements should fail" =
  run {| , |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse pattern tuple with 1 element should fail" =
  run {| 1, |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse pattern tuple with 2 elements" =
  run {| 1, 2 |};
  [%expect {|
      (1, 2) |}]
;;

let%expect_test "parse pattern tuple with 3 elements" =
  run {| 1, 2, myname |};
  [%expect {|
    (1, 2, myname) |}]
;;

let%expect_test "parse pattern tuples of tuples" =
  run {| (1, 2), (3, 4) |};
  [%expect {|
    ((1, 2), (3, 4)) |}]
;;

let%expect_test "parse pattern tuple of lists" =
  run {| ( [1; 2], [3; 4] ) |};
  [%expect {|
    ([1; 2], [3; 4])  |}]
;;

(************************** Lists **************************)

let%expect_test "parse pattern empty list" =
  run {| [] |};
  [%expect {| []  |}]
;;

let%expect_test "parse pattern list of 1 element" =
  run {| [a] |};
  [%expect {| [a]  |}]
;;

let%expect_test "parse pattern list of 2 element" =
  run {| [a; b] |};
  [%expect {| [a; b]  |}]
;;

let%expect_test "parse pattern list of list" =
  run {| [ [ 1; 2; 3] ] |};
  [%expect {|
    [[1; 2; 3]]  |}]
;;

let%expect_test "parse pattern list of tuples without parentheses" =
  run {| [ 1, 2; 3, 4 ] |};
  [%expect {|
    [(1, 2); (3, 4)]  |}]
;;

(************************** Parentheses **************************)

let%expect_test "parse const in parentheses as pattern const" =
  run {| ( 1 ) |};
  [%expect {| 1 |}]
;;

let%expect_test "parse pattern in unbalanced parentheses should fail" =
  run {| ( 1 ))) |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse pattern tuple in parentheses" =
  run {| (1, 2, 3, 4) |};
  [%expect {|
    (1, 2, 3, 4) |}]
;;

let%expect_test "parse pattern list in parentheses" =
  run {| ([1; 2; 3; 4]) |};
  [%expect {|
    [1; 2; 3; 4] |}]
;;

(************************** OR patterns **************************)

let%expect_test "parse OR pattern of two ident patterns" =
  run {| x | y |};
  [%expect {|
    x | y |}]
;;

let%expect_test "parse OR pattern of three ident patterns" =
  run {| x | y | z |};
  [%expect {|
    x | y | z |}]
;;

let%expect_test "parse OR pattern of tuple and ident patterns" =
  run {| x | (y, z) |};
  [%expect {|
    x | (y, z) |}]
;;

let%expect_test "parse OR pattern of ident and tuple patterns" =
  run {| (x, y) | z |};
  [%expect {|
    (x, y) | z |}]
;;

(************************** Typed patterns **************************)

let%expect_test "parse pattern with builtin type" =
  run {| x : int |};
  [%expect {|
    (x : int) |}]
;;

let%expect_test "parse typed pattern without colon should fail" =
  run {| x int |};
  [%expect {|
    : end_of_input |}]
;;

let%expect_test "parse typed tuple" =
  run {| (x, b) : int * string |};
  [%expect {|
    ((x, b) : int * string) |}]
;;

let%expect_test "parse typed tuple of typed identificators should fail" =
  run {| (x : int, b : string) : int * string |};
  [%expect {|
    : no more choices |}]
;;
