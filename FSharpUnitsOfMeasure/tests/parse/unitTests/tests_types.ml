(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Pprint.Pp
open Parse.Types
open Pprint.Pprinter

let run = pp pprint_type ptype
let _ = run
(************************** Builtin types **************************)

(************************** Idents **************************)

let%expect_test "parse basic type identificator" =
  run {| int |};
  [%expect {| int |}]
;;

(************************** Functions **************************)

let%expect_test "parse function type of 2 builtin types" =
  run {| int -> int |};
  [%expect {| int -> int |}]
;;

let%expect_test "parse function type of 3 builtin types" =
  run {| int -> int -> int |};
  [%expect {|
    int -> int -> int |}]
;;

(************************** Tuples **************************)

let%expect_test "parse tuple type of 2 builtin types" =
  run {| int * int |};
  [%expect {| int * int |}]
;;

let%expect_test "parse tuple type of 3 builtin types" =
  run {| int * int * int |};
  [%expect {| int * int * int |}]
;;

let%expect_test "parse tuple type of 5 builtin types" =
  run {| int * int * int * int * int |};
  [%expect {|
    int * int * int * int * int |}]
;;

(************************** Parentheses **************************)

let%expect_test "parse builtin type in parentheses" =
  run {| (int) |};
  [%expect {| int |}]
;;

let%expect_test "parse 2-builtin-types-tuple in parentheses" =
  run {| (int * int) |};
  [%expect {| int * int |}]
;;

let%expect_test "parse tuple of tuple and int " =
  run {| (int * int) * int |};
  [%expect {|
    int * int * int |}]
;;

let%expect_test "parse tuple of int and tuple " =
  run {| int * (int * int) |};
  [%expect {|
    int * int * int |}]
;;

let%expect_test "parse tuple of int, tuple and int" =
  run {| int * (int * int) * int |};
  [%expect {|
    int * int * int * int |}]
;;

let%expect_test "parse function taking function and returning int" =
  run {| (int -> int) -> int |};
  [%expect {|
    (int -> int) -> int |}]
;;

let%expect_test "parse function taking int and returning function" =
  run {| int -> (int -> int) |};
  [%expect {|
    int -> int -> int |}]
;;

let%expect_test "parse function taking int and function and returning int" =
  run {| int -> (int -> int) -> int |};
  [%expect {|
    int -> (int -> int) -> int |}]
;;

let%expect_test "parse function taking tuple and returning function" =
  run {| (int * int) -> (int -> int) |};
  [%expect {|
    int * int -> int -> int |}]
;;
