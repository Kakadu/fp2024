(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Base
open Pprint.Pp
open Parse.Patterns
open Pprint.Pprinter

(************************** Idents **************************)

let%expect_test "single underscore should be parsed as a wildcard" =
  pp pprint_pat ppat {| _ |};
  [%expect {| _ |}]
;;

let%expect_test "parse ident as pattern ident" =
  pp pprint_pat ppat {| x |};
  [%expect {| x |}]
;;

let%expect_test "parse + inside parentheses as pattern ident" =
  pp pprint_pat ppat {| (+) |};
  [%expect {| + |}]
;;

let%expect_test "parse +. operator inside parentheses as pattern ident" =
  pp pprint_pat ppat {| (+.) |};
  [%expect {| +. |}]
;;

(************************** Consts **************************)

let%expect_test "parse const int as pattern const int" =
  pp pprint_pat ppat {| 1 |};
  [%expect {| 1 |}]
;;

(************************** Tuples **************************)

let%expect_test "parse pattern tuple with 0 elements should fail" =
  pp pprint_pat ppat {| , |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse pattern tuple with 1 element should fail" =
  pp pprint_pat ppat {| 1, |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse pattern tuple with 2 elements" =
  pp pprint_pat ppat {| 1, 2 |};
  [%expect {|
      (1, 2) |}]
;;

let%expect_test "parse pattern tuple with 3 elements" =
  pp pprint_pat ppat {| 1, 2, myname |};
  [%expect {|
    (1, 2, myname) |}]
;;

let%expect_test "parse pattern tuples of tuples" =
  pp pprint_pat ppat {| (1, 2), (3, 4) |};
  [%expect {|
    ((1, 2), (3, 4)) |}]
;;

let%expect_test "parse pattern tuple of lists" =
  pp pprint_pat ppat {| ( [1; 2], [3; 4] ) |};
  [%expect {|
    ([1; 2], [3; 4])  |}]
;;

(************************** Lists **************************)

let%expect_test "parse pattern empty list" =
  pp pprint_pat ppat {| [] |};
  [%expect {| []  |}]
;;

let%expect_test "parse pattern list of 1 element" =
  pp pprint_pat ppat {| [a] |};
  [%expect {| [a]  |}]
;;

let%expect_test "parse pattern list of 2 element" =
  pp pprint_pat ppat {| [a; b] |};
  [%expect {| [a; b]  |}]
;;

let%expect_test "parse pattern list of list" =
  pp pprint_pat ppat {| [ [ 1; 2; 3] ] |};
  [%expect {|
    [[1; 2; 3]]  |}]
;;

let%expect_test "parse pattern list of tuples without parentheses" =
  pp pprint_pat ppat {| [ 1, 2; 3, 4 ] |};
  [%expect {|
    [(1, 2); (3, 4)]  |}]
;;

(************************** Parentheses **************************)

let%expect_test "parse const in parentheses as pattern const" =
  pp pprint_pat ppat {| ( 1 ) |};
  [%expect {| 1 |}]
;;

let%expect_test "parse pattern in unbalanced parentheses should fail" =
  pp pprint_pat ppat {| ( 1 ))) |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse pattern tuple in parentheses" =
  pp pprint_pat ppat {| (1, 2, 3, 4) |};
  [%expect {|
    (1, 2, 3, 4) |}]
;;

let%expect_test "parse pattern list in parentheses" =
  pp pprint_pat ppat {| ([1; 2; 3; 4]) |};
  [%expect {|
    [1; 2; 3; 4] |}]
;;

(************************** OR patterns **************************)

let%expect_test "parse OR pattern of two ident patterns" =
  pp pprint_pat ppat {| x | y |};
  [%expect {|
    x | y |}]
;;

let%expect_test "parse OR pattern of three ident patterns" =
  pp pprint_pat ppat {| x | y | z |};
  [%expect {|
    x | y | z |}]
;;

let%expect_test "parse OR pattern of tuple and ident patterns" =
  pp pprint_pat ppat {| x | (y, z) |};
  [%expect {|
    x | (y, z) |}]
;;

let%expect_test "parse OR pattern of ident and tuple patterns" =
  pp pprint_pat ppat {| (x, y) | z |};
  [%expect {|
    (x, y) | z |}]
;;

(************************** Typed patterns **************************)

let%expect_test "parse pattern with builtin type" =
  pp pprint_pat ppat {| x : int |};
  [%expect {|
    (x : int) |}]
;;

let%expect_test "parse typed pattern without colon should fail" =
  pp pprint_pat ppat {| x int |};
  [%expect {|
    : end_of_input |}]
;;

let%expect_test "parse typed tuple" =
  pp pprint_pat ppat {| (x, b) : int * string |};
  [%expect {|
    ((x, b) : int * string) |}]
;;

let%expect_test "parse typed tuple of typed identificators should fail" =
  pp pprint_pat ppat {| (x : int, b : string) : int * string |};
  [%expect {|
    : no more choices |}]
;;
