(** Copyright 2021-2024, Ksenia Kotelnikova, Gleb Nasretdinov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = Ident of string

type binary_operator =
  | Binary_equal
  | Binary_unequal
  | Binary_less
  | Binary_less_or_equal
  | Binary_greater
  | Binary_greater_or_equal
  | Binary_plus
  | Binary_minus
  | Binary_multiply
  | Logical_or
  | Logical_and
  | Binary_divide
  | Binary_or_bitwise
  | Binary_xor_bitwise
  | Binary_and_bitwise

type expr =
  | Const of float
  | Variable of (*name*) ident
  | Bin_expr of (*oper*) binary_operator * (*fst*) expr * (*snd*) expr
  | If_then_else of (*condition*) expr * (*then body*) expr list * (*else body*) expr list
  | Function of (*name*) string option * (*args*) expr list * (*body*) expr list
  | Function_call of (*name*) string * (*args*) expr list
  | Let of (*name*) ident * (*value*) expr (* * (*body*) expr list *)
