(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Pp
open Parse.Types
open Pprint.Pprinter

(************************** Builtin types **************************)

(************************** Idents **************************)

let%expect_test "parse basic type identificator" =
  pp pprint_type parse_type {| int |};
  [%expect {| int |}]
;;

(************************** Functions **************************)

let%expect_test "parse function type of 2 builtin types" =
  pp pprint_type parse_type {| int -> int |};
  [%expect {| int -> int |}]
;;

let%expect_test "parse function type of 3 builtin types" =
  pp pprint_type parse_type {| int -> int -> int |};
  [%expect
    {|
    int -> int -> int |}]
;;

(************************** Tuples **************************)

let%expect_test "parse tuple type of 2 builtin types" =
  pp pprint_type parse_type {| int * int |};
  [%expect {| int * int |}]
;;

let%expect_test "parse tuple type of 3 builtin types" =
  pp pprint_type parse_type {| int * int * int |};
  [%expect
    {| int * int * int |}]
;;

let%expect_test "parse tuple type of 5 builtin types" =
  pp pprint_type parse_type {| int * int * int * int * int |};
  [%expect
    {|
    int * int * int * int * int |}]
;;

(************************** Parentheses **************************)

let%expect_test "parse builtin type in parentheses" =
  pp pprint_type parse_type {| (int) |};
  [%expect {| int |}]
;;

let%expect_test "parse 2-builtin-types-tuple in parentheses" =
  pp pprint_type parse_type {| (int * int) |};
  [%expect {| int * int |}]
;;

let%expect_test "parse tuple of tuple and int " =
  pp pprint_type parse_type {| (int * int) * int |};
  [%expect
    {|
    int * int * int |}]
;;

let%expect_test "parse tuple of int and tuple " =
  pp pprint_type parse_type {| int * (int * int) |};
  [%expect
    {|
    int * int * int |}]
;;

let%expect_test "parse tuple of int, tuple and int" =
  pp pprint_type parse_type {| int * (int * int) * int |};
  [%expect
    {|
    int * int * int * int |}]
;;

let%expect_test "parse function taking function and returning int" =
  pp pprint_type parse_type {| (int -> int) -> int |};
  [%expect
    {|
    (int -> int) -> int |}]
;;

let%expect_test "parse function taking int and returning function" =
  pp pprint_type parse_type {| int -> (int -> int) |};
  [%expect
    {|
    int -> int -> int |}]
;;

let%expect_test "parse function taking int and function and returning int" =
  pp pprint_type parse_type {| int -> (int -> int) -> int |};
  [%expect
    {|
    int -> (int -> int) -> int |}]
;;

let%expect_test "parse function taking tuple and returning function" =
  pp pprint_type parse_type {| (int * int) -> (int -> int) |};
  [%expect
    {|
    int * int -> int -> int |}]
;;
