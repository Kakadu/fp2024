(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.Ast
open FSharpActivePatterns.PrintAst

let%expect_test "print double func" =
  let recursive = Nonrec in
  let name = Some "double" in
  let var = Variable (Ident "n") in
  let args = [ var ] in
  let binary_expr = Bin_expr (Binary_multiply, Const (Int_lt 2), var) in
  let double = Function_def (recursive, name, args, binary_expr) in
  print_construction @@ Expr double;
  [%expect
    {|
    |  Function(double):
      ARGS
    ----| Variable(n)
      BODY
    ----| Binary expr(
    ----| Binary Multiply
    ------| Const(Int: 2)
    ------| Variable(n)|}]
;;

let%expect_test "print tuple of binary operators" =
  let first = Const (Int_lt 3) in
  let second = Const (Int_lt 10) in
  let operators =
    [ Binary_unequal
    ; Binary_less
    ; Binary_less_or_equal
    ; Binary_greater
    ; Binary_greater_or_equal
    ; Binary_add
    ; Logical_and
    ; Binary_divide
    ; Binary_or_bitwise
    ; Binary_xor_bitwise
    ; Binary_and_bitwise
    ]
  in
  let print_binary_constr operator =
    print_construction @@ Expr (Bin_expr (operator, first, second))
  in
  List.iter print_binary_constr operators;
  [%expect
    {|
    | Binary expr(
    | Binary Unequal
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Less
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Less Or Equal
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Greater
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Greater Or Equal
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Add
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Logical And
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Divide
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Or Bitwise
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Xor Bitwise
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary And Bitwise
    --| Const(Int: 3)
    --| Const(Int: 10) |}]
;;
