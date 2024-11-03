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
