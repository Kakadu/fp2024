(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Base
open Stdlib.Format
open Ast

type value =
  | ValInt of int
  | ValStr of string
  | ValBool of bool
  | ValUnit
  | ValList of value list
  | ValTup of value * value * value list
  | ValFun of rec_state * pat * pat list * expr * env
  | ValBuiltIn of string
  | ValOption of value option

and env = (id, value, String.comparator_witness) Map.t

type error =
  | NoVariable of string
  | TypeError

val pp_value : formatter -> value -> unit
val pp_error : formatter -> error -> unit
