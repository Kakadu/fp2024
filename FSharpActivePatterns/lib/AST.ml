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

type recursive =
  | Nonrec
  | Rec (**recursion flag*)

type expr =
  | Const of literal
  | Tuple of expr list
  | Variable of ident
  | Bin_expr of binary_operator * expr * expr (**operator, first operand, second operand*)
  | If_then_else of expr * expr list * expr list option
  (**condition, then body, else body*)
  | Function_def of recursive * string option * expr list * expr list (**rec, name, body*)
  | Function_dec of recursive * string * expr list (**rec, name, args*)
  | Function_call of string * expr list (**name, args*)
  | Let of ident * expr (**name, expr*)
