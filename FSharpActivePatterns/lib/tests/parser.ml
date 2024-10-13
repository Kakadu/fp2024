(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.Parser
open FSharpActivePatterns.PrintAst
open FSharpActivePatterns.Ast
open Angstrom

let parse (p : expr t) input =
  match parse_string ~consume:All p input with
  | Ok v -> Expr v
  | Error msg -> failwith msg
;;

let%expect_test "logical expr parsing" =
  let input = {| (3 + 5) >= 8 || true && (5 <> 4) |} in
  let result = parse bool_expr input in
  print_construction result;
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

let%expect_test "int_expr" =
  let input = " (3 + 5) - (12 / 7)" in
  let result = parse int_expr input in
  print_construction result;
  [%expect
    {|
    | Binary expr(
    | Binary Subtract
    --| Binary expr(
    --| Binary Add
    ----| Const(Int: 3)
    ----| Const(Int: 5)
    --| Binary expr(
    --| Binary Divide
    ----| Const(Int: 12)
    ----| Const(Int: 7) |}]
;;

let%expect_test "parse_unary_chain" =
  let input = "not not ( not true && false || 3 > 5)" in
  let result = parse (parse_unary_chain bool_expr log_not) input in
  print_construction result;
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

let%expect_test "parse_unary_chainl1" =
  let input = "if 3 > 2 && false then 5 + 7 else 12" in
  let result = parse parse_if input in
  print_construction result;
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
