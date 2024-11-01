(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Pp
open Parse.Patterns

(************************** Idents **************************)

let%expect_test "single underscore should be parsed as a wildcard" =
  pp pp_pattern parse_pat {| _ |};
  [%expect {| Pattern_wild |}]
;;

let%expect_test "parse ident as pattern ident" =
  pp pp_pattern parse_pat {| x |};
  [%expect {| (Pattern_ident "x") |}]
;;

(************************** Consts **************************)

let%expect_test "parse const int as pattern const int" =
  pp pp_pattern parse_pat {| 1 |};
  [%expect {| (Pattern_const (Const_int 1)) |}]
;;

(************************** Tuples **************************)

let%expect_test "parse pattern tuple with 0 elements should fail" =
  pp pp_pattern parse_pat {| , |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse pattern tuple with 1 element should fail" =
  pp pp_pattern parse_pat {| 1, |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse pattern tuple with 2 elements" =
  pp pp_pattern parse_pat {| 1, 2 |};
  [%expect
    {| (Pattern_tuple [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2))]) |}]
;;

let%expect_test "parse pattern tuple with 3 elements" =
  pp pp_pattern parse_pat {| 1, 2, myname |};
  [%expect
    {|
    (Pattern_tuple
       [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2));
         (Pattern_ident "myname")]) |}]
;;

let%expect_test "parse tuples of tuples" =
  pp pp_pattern parse_pat {| (1, 2), (3, 4) |};
  [%expect
    {|
    (Pattern_tuple
       [(Pattern_tuple
           [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2))]);
         (Pattern_tuple
            [(Pattern_const (Const_int 3)); (Pattern_const (Const_int 4))])
         ]) |}]
;;

let%expect_test "parse tuple of lists" =
  pp pp_pattern parse_pat {| ( [1; 2], [3; 4] ) |};
  [%expect {|
    (Pattern_tuple
       [(Pattern_list
           [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2))]);
         (Pattern_list
            [(Pattern_const (Const_int 3)); (Pattern_const (Const_int 4))])
         ])  |}]
;;

(************************** Lists **************************)

let%expect_test "parse empty list pattern" =
  pp pp_pattern parse_pat {| [] |};
  [%expect {| (Pattern_list [])  |}]
;;

let%expect_test "parse list pattern of 1 element" =
  pp pp_pattern parse_pat {| [a] |};
  [%expect {| (Pattern_list [(Pattern_ident "a")])  |}]
;;

let%expect_test "parse list pattern of 2 element" =
  pp pp_pattern parse_pat {| [a; b] |};
  [%expect {| (Pattern_list [(Pattern_ident "a"); (Pattern_ident "b")])  |}]
;;

let%expect_test "parse list of list" =
  pp pp_pattern parse_pat {| [ [ 1; 2; 3] ] |};
  [%expect {|
    (Pattern_list
       [(Pattern_list
           [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2));
             (Pattern_const (Const_int 3))])
         ])  |}]
;;

let%expect_test "parse list of tuples without parentheses" =
  pp pp_pattern parse_pat {| [ 1, 2; 3, 4 ] |};
  [%expect {|
    (Pattern_list
       [(Pattern_tuple
           [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2))]);
         (Pattern_tuple
            [(Pattern_const (Const_int 3)); (Pattern_const (Const_int 4))])
         ])  |}]
;;

(************************** Parentheses **************************)

let%expect_test "parse const in parentheses as pattern const" =
  pp pp_pattern parse_pat {| ( 1 ) |};
  [%expect {| (Pattern_const (Const_int 1)) |}]
;;

let%expect_test "parse pattern in unbalanced parentheses should fail" =
  pp pp_pattern parse_pat {| ( 1 ))) |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse tuple in parentheses" =
  pp pp_pattern parse_pat {| (1, 2, 3, 4) |};
  [%expect
    {|
    (Pattern_tuple
       [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2));
         (Pattern_const (Const_int 3)); (Pattern_const (Const_int 4))]) |}]
;;

let%expect_test "parse list in parentheses" =
  pp pp_pattern parse_pat {| ([1; 2; 3; 4]) |};
  [%expect
    {|
    (Pattern_list
       [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2));
         (Pattern_const (Const_int 3)); (Pattern_const (Const_int 4))]) |}]
;;
