(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Ast

type var_typ = int [@@deriving show { with_path = false }]

type constant_typ =
  | TInt
  | TStr
  | TBool
  | TUnit
[@@deriving show { with_path = false }]

type typ =
  | TypConst of constant_typ
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

(* let equal_const_typ =  *)
(* Type constructors *)
let int_cons = TypConst TInt
let bool_cons = TypConst TBool
let string_cons = TypConst TStr
let unit_cons = TypConst TUnit
let constant_cons ty = TypConst ty
let var_cons name = TypVar name
let arrow_cons ty1 ty2 = TypArrow (ty1, ty2)
let tup_cons ty1 ty2 ts = TypTuple (ty1, ty2, ts)
let list_cons ty = TypList ty
let option_cons ty = TypOption ty
