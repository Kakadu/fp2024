(* Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(* SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.Parser
open FSharpActivePatterns.PrintAst

let%expect_test "parse logical expression" =
  let input = {| (3 + 5) >= 8 || true && (5 <> 4) |} in
  let result = parse input in
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
    ----| Variable(true)
    ----| Binary expr(
    ----| Binary Unequal
    ------| Const(Int: 5)
    ------| Const(Int: 4) |}]
;;

let%expect_test "parse integer expression" =
  let input = " (3 + 5) - (12 / 7)" in
  let result = parse input in
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

(*
   let%expect_test "parse_unary_chain" =
  let input = "not not ( not true && false || 3 > 5)" in
  let result = parse input in
  print_construction result;
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.(fun) in file "lib/tests/parser.ml", line 52, characters 15-26
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  : end_of_input |}]
;; *)

let%expect_test "parse if with comparison" =
  let input = "if 3 > 2 && false then 5 + 7 else 12" in
  let result = parse input in
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
    ----| Variable(false)
      THEN BRANCH
    ----| Binary expr(
    ----| Binary Add
    ------| Const(Int: 5)
    ------| Const(Int: 7)
      ELSE BRANCH
    ----| Const(Int: 12) |}]
;;
