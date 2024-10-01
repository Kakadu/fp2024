(** Copyright 2024, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = Ident of string

type literal =
  | Int_lt of int
  | Bool_lt of bool
  | String_lt of string
  | Unit_lt

type binary_operator =
  | Binary_equal
  | Binary_unequal
  | Binary_less
  | Binary_less_or_equal
  | Binary_greater
  | Binary_greater_or_equal
  | Binary_add
  | Binary_subtract
  | Binary_multiply
  | Logical_or
  | Logical_and
  | Binary_divide
  | Binary_or_bitwise
  | Binary_xor_bitwise
  | Binary_and_bitwise

type unary_operator =
  | Unary_plus
  | Unary_minus
  | Unary_negative

type function_flag =
  | None
  | Rec (* recursion flag *)

type expr =
  | Const of literal
  | Tuple of expr list
  | Variable of (*name*) ident
  | Bin_expr of (*oper*) binary_operator * (*fst expr*) expr * (*snd expr*) expr
  | If_then_else of
      (*condition*) expr * (*then body*) expr list * (*else body*) expr list option
  | Function_def of
      (*special characteristic*) function_flag
      * (*name*)
      string option
      * (*args*)
      expr list
      * (*body*)
      expr list
  | Function_dec of
      (*special characteristic*) function_flag * (*name*) string * (*args*) expr list
  | Function_call of (*name*) string * (*args*) expr list
  | Let of (*name*) ident * (*value*) expr
