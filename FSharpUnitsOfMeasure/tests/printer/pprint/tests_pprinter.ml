(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Ast
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

(************************** Types **************************)

let%expect_test "print type int" =
  printf "%a\n" pprint_type (Type_ident "int");
  [%expect {| int |}]
;;

let%expect_test "print type int -> int" =
  printf "%a\n" pprint_type (Type_func (Type_ident "int", Type_ident "int"));
  [%expect {| int -> int |}]
;;

let%expect_test "print type int -> int -> int" =
  printf
    "%a\n"
    pprint_type
    (Type_func (Type_ident "int", Type_func (Type_ident "int", Type_ident "int")));
  [%expect {| int -> int -> int |}]
;;

let%expect_test "print type int -> (int -> int) and omit parentheses" =
  printf
    "%a\n"
    pprint_type
    (Type_func (Type_ident "int", Type_func (Type_ident "int", Type_ident "int")));
  [%expect {| int -> int -> int |}]
;;

let%expect_test "print type (int -> int) -> int" =
  printf
    "%a\n"
    pprint_type
    (Type_func (Type_func (Type_ident "int", Type_ident "int"), Type_ident "int"));
  [%expect {| (int -> int) -> int |}]
;;

let%expect_test "print type int * int" =
  printf "%a\n" pprint_type (Type_tuple (Type_ident "int", Type_ident "int", []));
  [%expect {| int * int |}]
;;

let%expect_test "print type int * int * int" =
  printf
    "%a\n"
    pprint_type
    (Type_tuple (Type_ident "int", Type_ident "int", [ Type_ident "int" ]));
  [%expect {| int * int * int |}]
;;

let%expect_test "print type int * int -> int" =
  printf
    "%a\n"
    pprint_type
    (Type_func (Type_tuple (Type_ident "int", Type_ident "int", []), Type_ident "int"));
  [%expect {| int * int -> int |}]
;;

(************************** Patterns **************************)

let%expect_test "print identificator pattern" =
  printf "%a\n" pprint_pat (Pattern_ident "ident");
  [%expect {| ident |}]
;;

let%expect_test "print int constant pattern" =
  printf "%a\n" pprint_pat (Pattern_const (Const_int 123));
  [%expect {| 123 |}]
;;

let%expect_test "print wildcard pattern" =
  printf "%a\n" pprint_pat Pattern_wild;
  [%expect {| _ |}]
;;

let%expect_test "print OR pattern" =
  printf "%a\n" pprint_pat (Pattern_or (Pattern_ident "P1", Pattern_ident "P2"));
  [%expect {| P1 | P2 |}]
;;

let%expect_test "print typed pattern x : int" =
  printf "%a\n" pprint_pat (Pattern_typed (Pattern_ident "x", Type_ident "int"));
  [%expect {| x : int |}]
;;

let%expect_test "print tuple pattern (x, y)" =
  printf "%a\n" pprint_pat (Pattern_tuple (Pattern_ident "x", Pattern_ident "y", []));
  [%expect {| (x, y) |}]
;;

let%expect_test "print tuple pattern (x, y, z)" =
  printf
    "%a\n"
    pprint_pat
    (Pattern_tuple (Pattern_ident "x", Pattern_ident "y", [ Pattern_ident "z" ]));
  [%expect {| (x, y, z) |}]
;;

let%expect_test "print tuple of typed idents pattern (x : int, y: int)" =
  printf
    "%a\n"
    pprint_pat
    (Pattern_tuple
       ( Pattern_typed (Pattern_ident "x", Type_ident "int")
       , Pattern_typed (Pattern_ident "y", Type_ident "int")
       , [] ));
  [%expect {| (x : int, y : int) |}]
;;

let%expect_test "print [] list pattern" =
  printf "%a\n" pprint_pat (Pattern_list []);
  [%expect {| [] |}]
;;

let%expect_test "print [x] list pattern" =
  printf "%a\n" pprint_pat (Pattern_list [ Pattern_ident "x" ]);
  [%expect {| [x] |}]
;;

let%expect_test "print [x; y] list pattern" =
  printf "%a\n" pprint_pat (Pattern_list [ Pattern_ident "x"; Pattern_ident "y" ]);
  [%expect {| [x; y] |}]
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
