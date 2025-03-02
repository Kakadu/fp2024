(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Ast

type var_typ = int

type constant_typ =
  | TInt
  | TStr
  | TBool
  | TUnit

(* val equal_const_typ : const_typ -> const_typ -> bool *)

type typ =
  | TypConst of constant_typ
  | TypVar of var_typ
  | TypArrow of typ * typ
  | TypTuple of typ * typ * typ list
  | TypList of typ
  | TypOption of typ

type error =
  | OccursCheckFailed of int * typ
  | UnificationFailed of typ * typ
  | UnboundVariable of id
  | InvalidRecursivePattern

val int_cons : typ
val bool_cons : typ
val string_cons : typ
val unit_cons : typ
val constant_cons : constant_typ -> typ
val var_cons : var_typ -> typ
val arrow_cons : typ -> typ -> typ
val tup_cons : typ -> typ -> typ list -> typ
val list_cons : typ -> typ
val option_cons : typ -> typ
