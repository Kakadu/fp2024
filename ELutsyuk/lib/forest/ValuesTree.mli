(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Stdlib.Format
open Ast

type value =
  | ValInt of int
  | ValStr of string
  | ValBool of bool
  | ValFun of pat * pat list * expr
  | ValTup of value list
  | ValBuiltIn of string
  | ValOption of id * value option

type error =
  | NoVariable of string
  | TypeError

val pp_value : Format.formatter -> value -> unit
val pp_error : Format.formatter -> error -> unit
