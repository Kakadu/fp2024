(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base
open Pprinter
open Format

let%expect_test "print identificator" =
  printf "%a\n" pprint_ident "myident";
  [%expect {| myident |}]
;;

(************************** Constants **************************)

let%expect_test "print int constant" =
  printf "%a\n" pprint_const (Const_int 123);
  [%expect {| 123 |}]
;;

let%expect_test "print char constant" =
  printf "%a\n" pprint_const (Const_char 'a');
  [%expect {| 'a' |}]
;;

let%expect_test "print string constant" =
  printf "%a\n" pprint_const (Const_string "gorillaz");
  [%expect {| "gorillaz" |}]
;;

let%expect_test "print float constant" =
  printf "%a\n" pprint_const (Const_float 3.142222222);
  [%expect {| 3.142222 |}]
;;

(************************** Patterns **************************)

let%expect_test "print wildcard pattern" =
  printf "%a\n" pprint_pat Pattern_wild;
  [%expect {| _ |}]
;;

let%expect_test "print identificator pattern" =
  printf "%a\n" pprint_pat (Pattern_ident "ident");
  [%expect {| ident |}]
;;

let%expect_test "print int constant pattern" =
  printf "%a\n" pprint_pat (Pattern_const (Const_int 123));
  [%expect {| 123 |}]
;;

let%expect_test "print OR pattern" =
  printf "%a\n" pprint_pat (Pattern_or (Pattern_ident "P1", Pattern_ident "P2"));
  [%expect {| P1 | P2 |}]
;;

(************************** Expressions **************************)

let%expect_test "print int constant expression" =
  printf "%a\n" pprint_expr (Expr_const (Const_int 123));
  [%expect {| 123 |}]
;;

let%expect_test "print identificator expression" =
  printf "%a\n" pprint_expr (Expr_ident_or_op "x");
  [%expect {| x |}]
;;

let%expect_test "print lambda expression" =
  printf "%a\n" pprint_expr (Expr_fun (Pattern_ident "x", Expr_ident_or_op "x"));
  [%expect {| fun x -> x |}]
;;

let%expect_test "print ite expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_ifthenelse (Expr_const (Const_bool true), Expr_ident_or_op "a", None));
  [%expect {| if true then a |}]
;;
