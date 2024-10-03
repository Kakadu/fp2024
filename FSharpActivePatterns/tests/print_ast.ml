(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open! FSharpActivePatterns.Ast
open! FSharpActivePatterns.PrintAst

let%expect_test "print double func" =
  let recursive = Nonrec in
  let name = Some "double" in
  let var = Variable (Ident "n") in
  let args = [ var ] in
  let binary_expr = Bin_expr (Binary_multiply, Const (Int_lt 2), var) in
  let double = Function_def (recursive, name, args, [ binary_expr ]) in
  print_expr 0 double;
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
  let binary_operators =
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
  List.iter (print_bin_op 0) binary_operators;
  [%expect
    {|
    | Binary Unequal
    | Binary Less
    | Binary Less Or Equal
    | Binary Greater
    | Binary Greater Or Equal
    | Binary Add
    | Logical And
    | Binary Divide
    | Binary Or Bitwise
    | Binary Xor Bitwise
    | Binary And Bitwise |}]
;;
