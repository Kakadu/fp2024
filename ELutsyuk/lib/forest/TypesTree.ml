(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

type var = int [@@deriving show { with_path = false }]

type constant =
  | TInt
  | TStr
  | TBool
  | TUnit
[@@deriving show { with_path = false }]

type typ =
  | TypConst of constant
  | TypVar of var
  | TypArrow of typ * typ
  | TypTuple of typ list
  | TypList of typ
  | TypOption of typ
[@@deriving show { with_path = false }]

type error =
  | OccursCheckFailed of int * typ
  | UnificationFailed of typ * typ
  | UnboundVariable of string
  | InvalidRecursivePattern
[@@deriving show { with_path = false }]

(* Type constructors *)
let int_typ = TypConst TInt
let bool_typ = TypConst TBool
let string_typ = TypConst TStr
let unit_typ = TypConst TUnit
let constant_typ ty = TypConst ty
let var_typ name = TypVar name
let arrow_typ ty1 ty2 = TypArrow (ty1, ty2)
let tup_typ ty = TypTuple ty
let list_typ ty = TypList ty
let option_typ ty = TypOption ty
