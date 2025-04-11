(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast

type error =
  | Division_by_zero
  | Type_mismatch
  | Unbound_identificator of string
  | Unsupported_operation of string
  | Match_failure
  | Not_implemented

type builtin_fun =
  | Print_int of (int -> unit)
  | Print_float of (float -> unit)
  | Print_string of (string -> unit)
  | Print_char of (char -> unit)
  | Print_bool of (bool -> unit)
  | Print_endline of (string -> unit)

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VChar of char
  | VString of string
  | VFun of rec_flag * pattern * expression * environment
  | VList of value list
  | VTuple of value * value * value list
  | VOption of value option
  | VFunction of rule * rule list
  | VBuiltin_fun of builtin_fun
  | VUnit

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

val pp_error : Format.formatter -> error -> unit
val pp_value : Format.formatter -> value -> unit
