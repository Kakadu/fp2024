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
  let double = Function_def (recursive, name, args, [ binary_expr ]) in
  print_expr double;
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

let%expect_test "print tu ple of binary operators" =
  let binary_operations =
    [ Bin_expr (Binary_unequal, Const (Int_lt 3), Const (Int_lt 10))
    ; Bin_expr (Binary_less, Const (Int_lt 3), Const (Int_lt 10))
    ; Bin_expr (Binary_less_or_equal, Const (Int_lt 10), Const (Int_lt (-45)))
    ; Bin_expr (Binary_greater, Const (Int_lt 3), Const (Int_lt 10))
    ; Bin_expr (Binary_greater_or_equal, Const (Int_lt 3), Const (Int_lt 10))
    ; Bin_expr (Binary_add, Const (Int_lt 3), Const (Int_lt 10))
    ; Bin_expr (Logical_and, Const (Int_lt 3), Const (Int_lt 10))
    ; Bin_expr (Binary_divide, Const (Int_lt 3), Const (Int_lt 10))
    ; Bin_expr (Binary_or_bitwise, Const (Int_lt 3), Const (Int_lt 10))
    ; Bin_expr (Binary_xor_bitwise, Const (Int_lt 3), Const (Int_lt 10))
    ; Bin_expr (Binary_and_bitwise, Const (Int_lt 3), Const (Int_lt 10))
    ]
  in
  List.iter print_expr binary_operations;
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
    --| Const(Int: 10)
    --| Const(Int: -45)
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
