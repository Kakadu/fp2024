(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Ast

type var = int

type constant =
  | TInt
  | TStr
  | TBool
  | TUnit

type typ =
  | TypConst of constant
  | TypVar of var
  | TypArrow of typ * typ
  | TypTuple of typ list
  | TypList of typ
  | TypOption of typ

type error =
  | OccursCheckFailed of int * typ
  | UnificationFailed of typ * typ
  | UnboundVariable of id
  | InvalidRecursivePattern

val int_typ : typ
val bool_typ : typ
val string_typ : typ
val unit_typ : typ
val constant_typ : constant -> typ
val var_typ : var -> typ
val arrow_typ : typ -> typ -> typ
val tup_typ : typ list -> typ
val list_typ : typ -> typ
val option_typ : typ -> typ
