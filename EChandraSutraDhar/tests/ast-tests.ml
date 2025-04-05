(** Copyright 2024-2025, Ram Prosad Chandra Sutra Dhar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EChandraSutraDhar_lib
open Ast

let%expect_test "test_const" =
  let print_const c = print_endline (show_const c) in
  print_const (ConstInt 42);
  print_const (ConstBool true);
  print_const (ConstString "hello");
  [%expect {|
    (ConstInt 42)
    (ConstBool true)
    (ConstString "hello") |}]
;;

let%expect_test "test_bin_oper" =
  let print_bin_oper op = print_endline (show_bin_oper op) in
  print_bin_oper Plus;
  print_bin_oper And;
  print_bin_oper Equal;
  [%expect {|
    Plus
    And
    Equal |}]
;;

let%expect_test "test_unar_oper" =
  let print_unar_oper op = print_endline (show_unar_oper op) in
  print_unar_oper Negative;
  print_unar_oper Not;
  [%expect {|
    Negative
    Not |}]
;;

let%expect_test "test_pattern" =
  let print_pattern p = print_endline (show_pattern p) in
  print_pattern (PatVariable "x");
  print_pattern (PatConst (ConstInt 42));
  print_pattern PatAny;
  [%expect {|
    (PatVariable "x")
    (PatConst (ConstInt 42))
    PatAny |}]
;;

let%expect_test "test_ty" =
  let print_ty ty =
    let fmt = Format.std_formatter in
    pp_ty fmt ty;
    Format.print_newline ()
  in
  print_ty (TyPrim "int");
  print_ty (TyVar 1);
  print_ty (TyArrow (TyPrim "int", TyPrim "bool"));
  print_ty (TyTuple [ TyPrim "int"; TyPrim "bool" ]);
  [%expect {|
    int
    '1
    int -> bool
    (int * bool) |}]
;;

let%expect_test "test_expr" =
  let print_expr e = print_endline (show_expr e) in
  print_expr (ExpConst (ConstInt 42));
  print_expr (ExpBinOper (Plus, ExpConst (ConstInt 1), ExpConst (ConstInt 2)));
  print_expr (ExpLambda ([ PatVariable "x" ], ExpIdent "x"));
  [%expect
    {|
    (ExpConst (ConstInt 42))
    (ExpBinOper (Plus, (ExpConst (ConstInt 1)), (ExpConst (ConstInt 2))))
    (ExpLambda ([(PatVariable "x")], (ExpIdent "x"))) |}]
;;

let%expect_test "test_pp_ty_complex" =
  let print_ty ty =
    let fmt = Format.std_formatter in
    pp_ty fmt ty;
    Format.print_newline ()
  in
  (* Проверка вложенных типов *)
  print_ty (TyArrow (TyArrow (TyPrim "int", TyPrim "bool"), TyPrim "string"));
  print_ty (TyList (TyArrow (TyPrim "int", TyPrim "bool")));
  print_ty (TyOption (TyTuple [ TyPrim "int"; TyPrim "bool" ]));
  [%expect
    {|
    (int -> bool) -> string
    (int -> bool) list
    ((int * bool)) option |}]
;;

let%expect_test "test_pattern_complex" =
  let print_pattern p = print_endline (show_pattern p) in
  print_pattern (PatTuple (PatVariable "x", PatVariable "y", [ PatVariable "z" ]));
  print_pattern (PatType (PatVariable "x", TyPrim "int"));
  print_pattern (PatList [ PatVariable "x"; PatVariable "y" ]);
  [%expect
    {|
    (PatTuple ((PatVariable "x"), (PatVariable "y"), [(PatVariable "z")]))
    (PatType ((PatVariable "x"), (TyPrim "int")))
    (PatList [(PatVariable "x"); (PatVariable "y")]) |}]
;;
