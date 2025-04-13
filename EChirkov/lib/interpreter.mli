(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  | UnboundVariable of string
  | TypeMissmatch
  | DivisionByZero

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VTuple of value * value * value list
  | VFun of rec_flag * pattern * expression * environment
  | VFunMutual of rec_flag * pattern * expression * environment
  | VList of value list
  | VOption of value option
  | VUnit
  | VPrintInt

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

val interpret
  :  program
  -> ((string, value, Base.String.comparator_witness) Base.Map.t, error) result

val pp_error : error -> string
