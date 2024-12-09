(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Parser
open Ocamladt_lib.Ast
open Pprinter

let%expect_test "parse_lambda_fun" =
  pp pp_expression pexpr "fun x y -> x * y";
  [%expect
    {|
    (Exp_fun (((Pat_var "x"), [(Pat_var "y")]),
       (Exp_apply ((Exp_ident "*"),
          ((Exp_tuple ((Exp_ident "x"), (Exp_ident "y"), [])), [])))
       )) |}]
;;

let%expect_test "parse_apply_to_const" =
  pp pp_expression pexpr "f 5";
  [%expect
    {|
    (Exp_apply ((Exp_ident "f"), ((Exp_constant (Const_integer 5)), []))) |}]
;;

let%expect_test "parse_apply_to_const_par" =
  pp pp_expression pexpr "f(5)";
  [%expect
    {|
    (Exp_apply ((Exp_ident "f"), ((Exp_constant (Const_integer 5)), []))) |}]
;;

let%expect_test "parse_apply_to_var" =
  pp pp_expression pexpr "f x";
  [%expect {|
    (Exp_apply ((Exp_ident "f"), ((Exp_ident "x"), []))) |}]
;;

let%expect_test "" =
  pp pp_expression pexpr "fun x y -> x * y";
  [%expect
    {|
    (Exp_fun (((Pat_var "x"), [(Pat_var "y")]),
       (Exp_apply ((Exp_ident "*"),
          ((Exp_tuple ((Exp_ident "x"), (Exp_ident "y"), [])), [])))
       )) |}]
;;

let%expect_test "parse_function_pattern_matching" =
  pp pp_expression pexpr "function | x -> true | y -> false";
  [%expect {|
    Syntax error |}]
;;

let%expect_test "parse_if_then_else_stmt" =
  pp pp_expression pexpr "if a = 3 then 5 else x - 1";
  [%expect
    {|
    (Exp_if (
       (Exp_apply ((Exp_ident "="),
          ((Exp_tuple ((Exp_ident "a"), (Exp_constant (Const_integer 3)), [])),
           [])
          )),
       (Exp_constant (Const_integer 5)),
       (Some (Exp_apply ((Exp_ident "-"),
                ((Exp_tuple
                    ((Exp_ident "x"), (Exp_constant (Const_integer 1)), [])),
                 [])
                )))
       )) |}]
;;

let%expect_test "parse_if_then_stmt" =
  pp pp_expression pexpr "if x < 5 then f(5)";
  [%expect
    {|
    (Exp_if (
       (Exp_apply ((Exp_ident "<"),
          ((Exp_tuple ((Exp_ident "x"), (Exp_constant (Const_integer 5)), [])),
           [])
          )),
       (Exp_apply ((Exp_ident "f"), ((Exp_constant (Const_integer 5)), []))),
       None)) |}]
;;

let%expect_test "parse_let_rec" =
  pp pp_expression pexpr "let rec a = 2 in z";
  [%expect
    {|
    (Exp_let (Recursive,
       ({ pat = (Pat_var "a"); expr = (Exp_constant (Const_integer 2)) }, []),
       (Exp_ident "z"))) |}]
;;

let%expect_test "parse_mul_let" =
  pp pp_expression pexpr "let a = 2 and g = 7";
  [%expect {|
    Syntax error |}]
;;

let%expect_test "parse_match" =
  pp pp_expression pexpr "match q with a -> h | h -> a";
  [%expect
    {|
    (Exp_match ((Exp_ident "q"),
       ({ left = (Pat_var "a"); right = (Exp_ident "h") },
        [{ left = (Pat_var "h"); right = (Exp_ident "a") }])
       )) |}]
;;
