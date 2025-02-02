(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Ast

type var_typ = int [@@deriving show { with_path = false }]

type const_typ =
  | TInt
  | TStr
  | TBool
  | TUnit
[@@deriving show { with_path = false }]

type typ =
  | TypConst of const_typ
  | TypVar of var_typ
  | TypArrow of typ * typ
  | TypTuple of typ * typ * typ list
  | TypList of typ
  | TypOption of typ
[@@deriving show { with_path = false }]

type error =
  | OccursCheckFailed of int * typ
  | UnificationFailed of typ * typ
  | UnboundVariable of id
[@@deriving show { with_path = false }]

(* Type constructors *)
let int_typ = TypConst TInt
let bool_typ = TypConst TBool
let string_typ = TypConst TStr
let unit_typ = TypConst TUnit
let var_typ x = TypVar x
let arrow_typ left right = TypArrow (left, right)
let tup_typ ty1 ty2 typs = TypTuple (ty1, ty2, typs)
let list_typ x = TypList x
(* let option_typ x = TypOption *)
