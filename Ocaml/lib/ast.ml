(** Copyright 2021-2023, Daniil Kadochnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_expr =
  | TInt
  | TBool
  | TString
  | TTuple of type_expr list
  | TList of type_expr
  | TOption of type_expr
  | TFun of type_expr * type_expr
[@@deriving show { with_path = false }]

type pattern = PVar of string * type_expr option [@@deriving show { with_path = false }]

type binary_operator =
  | Add (* + *)
  | Sub (* - *)
  | Mul (* * *)
  | Div (* / *)
  | And (* && *)
  | Or (* || *)
  | Eq (* = *)
  | Neq (* <> *)
  | Lt (* < *)
  | Gt (* > *)
  | Le (* <= *)
  | Ge (* >= *)
[@@deriving show { with_path = false }]

type rec_flag =
  | Recursive
  | NonRecursive
[@@deriving show { with_path = false }]

type expr =
  | EInt of int
  | EBool of bool
  | EVar of string * type_expr option
  | EString of string
  | EBinOp of binary_operator * expr * expr
  | EApp of expr * expr
  | EFun of pattern * expr
  | ELet of rec_flag * (pattern * expr) list * expr option
  | EIf of expr * expr * expr
  | ETuple of expr list
  | EList of expr list
  | ESome of expr
  | ENone
[@@deriving show { with_path = false }]
