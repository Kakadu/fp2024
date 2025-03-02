(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EUsoltsev_lib
open Ast
open Parser
open Printf

let parse_test input =
  match parse input with
  | Ok ast -> printf "%s\n" (show_program ast)
  | Error fail -> printf "Ошибка: %s\n" fail
;;

let%expect_test "factorial" =
  parse_test "let rec factorial n = if n < 2 then 1 else n * factorial(n - 1);;";
  [%expect
    {|
  [(SValue (true,
      ((PatVariable "factorial"),
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
          ))),
      []))
    ]
|}]
;;

let%expect_test "fibonacci" =
  parse_test "let rec fibo n = if n < 2 then 1 else fibo(n - 1) + fibo(n - 2) ;;";
  [%expect
    {|
  [(SValue (true,
      ((PatVariable "fibo"),
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
          ))),
      []))
    ]
|}]
;;

let%expect_test "lambda_test" =
  parse_test "let add x = fun y -> x + y;;";
  [%expect
    {|
  [(SValue (false,
      ((PatVariable "add"),
       (ExpLambda ([(PatVariable "x")],
          (ExpLambda ([(PatVariable "y")],
             (ExpBinOper (Plus, (ExpIdent "x"), (ExpIdent "y")))))
          ))),
      []))
    ]
|}]
;;

let%expect_test "test_tuple" =
  parse_test "let x = (1, 2, true) in x;;";
  [%expect
    {|
  [(SEval
      (ExpLet (false,
         ((PatVariable "x"),
          (ExpTuple ((ExpConst (ConstInt 1)), (ExpConst (ConstInt 2)),
             [(ExpConst (ConstBool true))]))),
         [], (ExpIdent "x"))))
    ]
|}]
;;

let%expect_test "test_list" =
  parse_test "let arr = [1;2;true]";
  [%expect
    {|
     [(SValue (false,
         ((PatVariable "arr"),
          (ExpList
             [(ExpConst (ConstInt 1)); (ExpConst (ConstInt 2));
               (ExpConst (ConstBool true))])),
         []))
       ]
|}]
;;

let%expect_test "test_one_element_in_tuple" =
  parse_test "let x = (666)";
  [%expect
    {|
     [(SValue (false, ((PatVariable "x"), (ExpConst (ConstInt 666))), []))]
|}]
;;

let%expect_test "test_sum_two_args" =
  parse_test "let sum x y = x + y";
  [%expect
    {|
[(SValue (false,
    ((PatVariable "sum"),
     (ExpLambda ([(PatVariable "x"); (PatVariable "y")],
        (ExpBinOper (Plus, (ExpIdent "x"), (ExpIdent "y")))))),
    []))
  ]
|}]
;;

let%expect_test "test_annotate_type_1" =
  parse_test "let sum (x:int) (y:int) = x + y";
  [%expect
    {|
[(SValue (false,
    ((PatVariable "sum"),
     (ExpLambda (
        [(PatType ((PatVariable "x"), (TyPrim "int")));
          (PatType ((PatVariable "y"), (TyPrim "int")))],
        (ExpBinOper (Plus, (ExpIdent "x"), (ExpIdent "y")))))),
    []))
  ]
|}]
;;

let%expect_test "test_annotate_type_2" =
  parse_test "let (a : int list) = [] ";
  [%expect
    {|
[(SValue (false,
    ((PatType ((PatVariable "a"), (TyList (TyPrim "int")))), (ExpList [])),
    []))
  ]
|}]
;;

let%expect_test "test_minus" =
  parse_test "-1 -2 - (-1) -(3)";
  [%expect
    {|
[(SEval
    (ExpBinOper (Minus,
       (ExpBinOper (Minus,
          (ExpBinOper (Minus,
             (ExpUnarOper (Negative, (ExpConst (ConstInt 1)))),
             (ExpConst (ConstInt 2)))),
          (ExpUnarOper (Negative, (ExpConst (ConstInt 1)))))),
       (ExpConst (ConstInt 3)))))
  ]
 |}]
;;

let%expect_test "test_unit" =
  parse_test "let () = print_int 5";
  [%expect
    {|
[(SValue (false,
    (PatUnit, (ExpFunction ((ExpIdent "print_int"), (ExpConst (ConstInt 5))))),
    []))
  ]
 |}]
;;
