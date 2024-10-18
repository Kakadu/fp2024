(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.Parser
open FSharpActivePatterns.PrintAst

let%expect_test "parse logical expression" =
  let input = {| (3 + 5) >= 8 || true && (5 <> 4) |} in
  print_p_res (parse input);
  [%expect
    {|
    | Binary expr(
    | Logical Or
    --| Binary expr(
    --| Binary Greater Or Equal
    ----| Binary expr(
    ----| Binary Add
    ------| Const(Int: 3)
    ------| Const(Int: 5)
    ----| Const(Int: 8)
    --| Binary expr(
    --| Logical And
    ----| Const(Bool: true)
    ----| Binary expr(
    ----| Binary Unequal
    ------| Const(Int: 5)
    ------| Const(Int: 4) |}]
;;

let%expect_test "parse integer expression" =
  let input = " (3 + 5) - (12 / 7)" in
  print_p_res (parse input);
  [%expect
    {|
    | Function Call:
      FUNCTION
    --| Binary expr(
    --| Binary Add
    ----| Const(Int: 3)
    ----| Const(Int: 5)
      ARGS
    --| Unary expr(
    --| Unary minus
    ----| Binary expr(
    ----| Binary Divide
    ------| Const(Int: 12)
    ------| Const(Int: 7) |}]
;;

let%expect_test "parse_unary_chain" =
  let input = "not not ( not true && false || 3 > 5)" in
  let result = parse input in
  print_p_res result;
  [%expect
    {|
    | Unary expr(
    | Unary negative
    --| Unary expr(
    --| Unary negative
    ----| Binary expr(
    ----| Logical Or
    ------| Binary expr(
    ------| Logical And
    --------| Unary expr(
    --------| Unary negative
    ----------| Const(Bool: true)
    --------| Const(Bool: false)
    ------| Binary expr(
    ------| Binary Greater
    --------| Const(Int: 3)
    --------| Const(Int: 5) |}]
;;

let%expect_test "parse if with comparison" =
  let input = "if 3 > 2 && false then 5 + 7 else 12" in
  print_p_res (parse input);
  [%expect
    {|
    | If Then Else(
      CONDITION
    --| Binary expr(
    --| Logical And
    ----| Binary expr(
    ----| Binary Greater
    ------| Const(Int: 3)
    ------| Const(Int: 2)
    ----| Const(Bool: false)
      THEN BRANCH
    ----| Binary expr(
    ----| Binary Add
    ------| Const(Int: 5)
    ------| Const(Int: 7)
      ELSE BRANCH
    ----| Const(Int: 12) |}]
;;

let%expect_test "sum with if" =
  let input = "a + if 3 > 2 then 2 else 1" in
  print_p_res (parse input);
  [%expect
    {|
    | Binary expr(
    | Binary Add
    --| Variable(a)
    --| If Then Else(
        CONDITION
    ----| Binary expr(
    ----| Binary Greater
    ------| Const(Int: 3)
    ------| Const(Int: 2)
        THEN BRANCH
    ------| Const(Int: 2)
        ELSE BRANCH
    ------| Const(Int: 1) |}]
;;

let%expect_test "inner expressions with LetIn and If" =
  let input =
    "if let x = true in let y = false in x || y then 3 else if 5 > 3 then 2 else 1"
  in
  print_p_res (parse input);
  [%expect
    {|
    | If Then Else(
      CONDITION
    -- | LetIn  x =
        ARGS
    ----| No args
        BODY
    ----| Const(Bool: true)
        INNER EXPRESSION
    ---- | LetIn  y =
          ARGS
    ------| No args
          BODY
    ------| Const(Bool: false)
          INNER EXPRESSION
    ------| Binary expr(
    ------| Logical Or
    --------| Variable(x)
    --------| Variable(y)
      THEN BRANCH
    ----| Const(Int: 3)
      ELSE BRANCH
    ----| If Then Else(
          CONDITION
    ------| Binary expr(
    ------| Binary Greater
    --------| Const(Int: 5)
    --------| Const(Int: 3)
          THEN BRANCH
    --------| Const(Int: 2)
          ELSE BRANCH
    --------| Const(Int: 1) |}]
;;
