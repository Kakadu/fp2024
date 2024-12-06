(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EUsoltsev_lib
open Ast
open Parser
open Printf
open Inferencer
open Typing

let parse_test input =
  match parse input with
  | Ok ast -> printf "%s\n" (show_program ast)
  | Error fail -> printf "Ошибка: %s\n" fail
;;

let pp_infer e =
  match run_inference e with
  | Ok ty -> Stdlib.Format.printf "%a" pp_ty ty
  | Error err -> Stdlib.Format.printf "%a" pp_error err
;;

let pp_parse_expr_and_infer input =
  match Parser.parse_string_expr input with
  | Ok e -> pp_infer e
  | Error _ -> Stdlib.print_endline "Failed to parse"
;;

(* parser *)

let%expect_test "factorial" =
  parse_test "let rec factorial n = if n < 2 then 1 else n * factorial(n - 1);;";
  [%expect
    {|
  [(ExpLet (true, (PatVariable "factorial"),
      (ExpLambda ([(PatVariable "n")],
         (ExpBranch (
            (ExpBinOper (LowerThan, (ExpIdent "n"), (ExpConst (ConstInt 2)))),
            (ExpConst (ConstInt 1)),
            (Some (ExpBinOper (Multiply, (ExpIdent "n"),
                     (ExpFunction ((ExpIdent "factorial"),
                        (ExpBinOper (Minus, (ExpIdent "n"),
                           (ExpConst (ConstInt 1))))
                        ))
                     )))
            ))
         )),
      None))
    ]  
|}]
;;

let%expect_test "fibonacci" =
  parse_test "let rec fibo n = if n < 2 then 1 else fibo(n - 1) + fibo(n - 2) ;;";
  [%expect
    {|
  [(ExpLet (true, (PatVariable "fibo"),
      (ExpLambda ([(PatVariable "n")],
         (ExpBranch (
            (ExpBinOper (LowerThan, (ExpIdent "n"), (ExpConst (ConstInt 2)))),
            (ExpConst (ConstInt 1)),
            (Some (ExpBinOper (Plus,
                     (ExpFunction ((ExpIdent "fibo"),
                        (ExpBinOper (Minus, (ExpIdent "n"),
                           (ExpConst (ConstInt 1))))
                        )),
                     (ExpFunction ((ExpIdent "fibo"),
                        (ExpBinOper (Minus, (ExpIdent "n"),
                           (ExpConst (ConstInt 2))))
                        ))
                     )))
            ))
         )),
      None))
    ]
|}]
;;

let%expect_test "double_let" =
  parse_test "let x = 10 + 19 * 20 ;; let y = 9 * 1 / 8000 ;;";
  [%expect
    {|
  [(ExpLet (false, (PatVariable "x"), 
      (ExpBinOper (Plus, (ExpConst (ConstInt 10)),
         (ExpBinOper (Multiply, (ExpConst (ConstInt 19)),
            (ExpConst (ConstInt 20))))
         )),
      None));
    (ExpLet (false, (PatVariable "y"),
       (ExpBinOper (Division,
          (ExpBinOper (Multiply, (ExpConst (ConstInt 9)),
             (ExpConst (ConstInt 1)))),
          (ExpConst (ConstInt 8000)))),
       None))
    ]
|}]
;;

let%expect_test "lambda_test" =
  parse_test "let add x = fun y -> x + y;;";
  [%expect
    {|
  [(ExpLet (false, (PatVariable "add"),
      (ExpLambda ([(PatVariable "x")],
         (ExpLambda ([(PatVariable "y")],
            (ExpBinOper (Plus, (ExpIdent "x"), (ExpIdent "y")))))
         )),
      None))
    ]
|}]
;;

let%expect_test "test_tuple" =
  parse_test "let x = (1, 2, true) in x;;";
  [%expect
    {|
  [(ExpLet (false, (PatVariable "x"), 
      (ExpTuple ((ExpConst (ConstInt 1)), (ExpConst (ConstInt 2)),
         [(ExpConst (ConstBool true))])),
      (Some (ExpIdent "x"))))
    ]
|}]
;;

let%expect_test "test_list" =
  parse_test "let arr = [1;2;true]";
  [%expect
    {|
     [(ExpLet (false, (PatVariable "arr"),
         (ExpList
            [(ExpConst (ConstInt 1)); (ExpConst (ConstInt 2));
              (ExpConst (ConstBool true))]),
         None))
       ]
|}]
;;

let%expect_test "test_one_element_in_tuple" =
  parse_test "let x = (666)";
  [%expect
    {|
     [(ExpLet (false, (PatVariable "x"), (ExpConst (ConstInt 666)), None))]
|}]
;;

let%expect_test "test_sum_two_args" =
  parse_test "let sum x y = x + y";
  [%expect
    {|
[(ExpLet (false, (PatVariable "sum"),
    (ExpLambda ([(PatVariable "x"); (PatVariable "y")],
       (ExpBinOper (Plus, (ExpIdent "x"), (ExpIdent "y"))))),
    None))
  ]
|}]
;;

(* inferencer *)

let%expect_test "test_binary_oper" =
  pp_parse_expr_and_infer "10/2 + 56*2 - 10 / 10 / 20 + 666 - 777 + 1";
  [%expect {|int|}]
;;

let%expect_test "test_bool" =
  pp_parse_expr_and_infer "false";
  [%expect {|bool|}]
;;

let%expect_test "test_string" =
  pp_parse_expr_and_infer "\"I like OCaml\" ";
  [%expect {|string|}]
;;

let%expect_test "test_option" =
  pp_parse_expr_and_infer "Some 10";
  [%expect {|int option|}]
;;

let%expect_test "test_binary_oper_and_arg" =
  pp_parse_expr_and_infer "fun x -> x * 69 + 100 - 201 / 777";
  [%expect {|a -> int|}]
;;

let%expect_test "test_binary_oper_and_arg" =
  pp_parse_expr_and_infer "fun x -> x * 69 + 100 - 201 / 777";
  [%expect {|a -> int|}]
;;

let%expect_test "test_rec" =
  pp_parse_expr_and_infer "let rec func arg = func arg";
  [%expect {|b -> c|}]
;;

let%expect_test "test_func_apply_some_args" =
  pp_parse_expr_and_infer "let func a1 a2 a3 = a1 a2 a3";
  [%expect {|a -> b -> c -> e|}]
;;

let%expect_test "test_tuple" =
  pp_parse_expr_and_infer "fun x y z -> (x + 10, y / 2 , z)";
  [%expect {|a -> b -> c -> (int * int * c)|}]
;;

let%expect_test "test_list" =
  pp_parse_expr_and_infer "let arr = [1;2;3]";
  [%expect {|int list|}]
;;

let%expect_test "test_binary_oper" =
  pp_parse_expr_and_infer "let is_above_10 x = if x > 10 then true else false ";
  [%expect {|a -> bool|}]
;;

let%expect_test "test_binary_oper" =
  pp_parse_expr_and_infer "let is_above_10 x = x > 10";
  [%expect {|a -> bool|}]
;;

let%expect_test "test_factorial" =
  pp_parse_expr_and_infer "let rec fac n = if n < 2 then 1 else n * fac (n - 1)";
  [%expect {|int -> int|}]
;;

let%expect_test "test_fibonacci" =
  pp_parse_expr_and_infer
    "let rec fibo n = if n < 2 then 1 else fibo(n - 1) + fibo(n - 2)";
  [%expect {|int -> int|}]
;;

let%expect_test "test_unbound_var" =
  pp_parse_expr_and_infer "let f = x";
  [%expect {|Error: Unbound variable 'x'.|}]
;;

let%expect_test "test_unification_types" =
  pp_parse_expr_and_infer "fun x -> x + true";
  [%expect {|Error: Failed to unify types: bool and int.|}]
;;
