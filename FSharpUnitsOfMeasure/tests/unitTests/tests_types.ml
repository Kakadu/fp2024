(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Pp
open Parse.Types

(************************** Builtin types **************************)

(************************** Idents **************************)

let%expect_test "parse basic type identificator" =
  pp pp_core_type parse_type {| int |};
  [%expect {| (Type_ident "int") |}]
;;

(************************** Functions **************************)

let%expect_test "parse function type of 2 builtin types" =
  pp pp_core_type parse_type {| int -> int |};
  [%expect {| (Type_func ((Type_ident "int"), (Type_ident "int"))) |}]
;;

let%expect_test "parse function type of 3 builtin types" =
  pp pp_core_type parse_type {| int -> int -> int |};
  [%expect
    {|
    (Type_func ((Type_ident "int"),
       (Type_func ((Type_ident "int"), (Type_ident "int"))))) |}]
;;

(************************** Tuples **************************)

let%expect_test "parse tuple type of 2 builtin types" =
  pp pp_core_type parse_type {| int * int |};
  [%expect {| (Type_tuple ((Type_ident "int"), (Type_ident "int"), [])) |}]
;;

let%expect_test "parse tuple type of 3 builtin types" =
  pp pp_core_type parse_type {| int * int * int |};
  [%expect
    {| (Type_tuple ((Type_ident "int"), (Type_ident "int"), [(Type_ident "int")])) |}]
;;

let%expect_test "parse tuple type of 5 builtin types" =
  pp pp_core_type parse_type {| int * int * int * int * int |};
  [%expect
    {|
    (Type_tuple ((Type_ident "int"), (Type_ident "int"),
       [(Type_ident "int"); (Type_ident "int"); (Type_ident "int")])) |}]
;;

(************************** Parentheses **************************)

let%expect_test "parse builtin type in parentheses" =
  pp pp_core_type parse_type {| (int) |};
  [%expect {| (Type_ident "int") |}]
;;

let%expect_test "parse 2-builtin-types-tuple in parentheses" =
  pp pp_core_type parse_type {| (int * int) |};
  [%expect {| (Type_tuple ((Type_ident "int"), (Type_ident "int"), [])) |}]
;;

let%expect_test "parse tuple of tuple and int " =
  pp pp_core_type parse_type {| (int * int) * int |};
  [%expect
    {|
    (Type_tuple ((Type_tuple ((Type_ident "int"), (Type_ident "int"), [])),
       (Type_ident "int"), [])) |}]
;;

let%expect_test "parse tuple of int and tuple " =
  pp pp_core_type parse_type {| int * (int * int) |};
  [%expect
    {|
    (Type_tuple ((Type_ident "int"),
       (Type_tuple ((Type_ident "int"), (Type_ident "int"), [])), [])) |}]
;;

let%expect_test "parse tuple of int, tuple and int" =
  pp pp_core_type parse_type {| int * (int * int) * int |};
  [%expect
    {|
    (Type_tuple ((Type_ident "int"),
       (Type_tuple ((Type_ident "int"), (Type_ident "int"), [])),
       [(Type_ident "int")])) |}]
;;

let%expect_test "parse function taking function and returning int" =
  pp pp_core_type parse_type {| (int -> int) -> int |};
  [%expect
    {|
    (Type_func ((Type_func ((Type_ident "int"), (Type_ident "int"))),
       (Type_ident "int"))) |}]
;;

let%expect_test "parse function taking int and returning function" =
  pp pp_core_type parse_type {| int -> (int -> int) |};
  [%expect
    {|
    (Type_func ((Type_ident "int"),
       (Type_func ((Type_ident "int"), (Type_ident "int"))))) |}]
;;

let%expect_test "parse function taking int and function and returning int" =
  pp pp_core_type parse_type {| int -> (int -> int) -> int |};
  [%expect
    {|
    (Type_func ((Type_ident "int"),
       (Type_func ((Type_func ((Type_ident "int"), (Type_ident "int"))),
          (Type_ident "int")))
       )) |}]
;;

let%expect_test "parse function taking tuple and returning function" =
  pp pp_core_type parse_type {| (int * int) -> (int -> int) |};
  [%expect
    {|
    (Type_func ((Type_tuple ((Type_ident "int"), (Type_ident "int"), [])),
       (Type_func ((Type_ident "int"), (Type_ident "int"))))) |}]
;;
