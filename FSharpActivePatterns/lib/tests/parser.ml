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
