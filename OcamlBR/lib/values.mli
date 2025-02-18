(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type builtin =
  | BInt of (int -> unit)
  | BString of (string -> unit)

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VList of value list
  | VTuple of value * value * value list
  | VFun of Ast.rec_flag * Ast.pattern * Ast.pattern list * Ast.expr * environment
  | VOption of value option
  | VBuiltin of builtin
  | VFunction of Ast.case * Ast.case list

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

val pp_value : Format.formatter -> value -> unit

type error =
  [ `Division_by_zero
  | `Unbound_variable of string
  | `Pattern_matching_failure
  | `Type_error
  | `Ill_left_hand_side of string
  ]

val pp_error : Format.formatter -> error -> unit
