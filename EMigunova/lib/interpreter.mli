(** Copyright 2025, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  [ `Type_error
  | `Division_by_zero
  | `Match_failure
  | `Too_many_args_for_anonym_fun
  | `Too_many_args_for_fun of string
  | `No_variable of string
  ]

type value =
  | Val_integer of int
  | Val_char of char
  | Val_string of string
  | Val_unit
  | Val_bool of bool
  | Val_fun of rec_flag * ident list option * pattern list * expression * env
  | Val_function of (pattern * expression) list * env
  | Val_tuple of value list
  | Val_list of value list
  | Val_option of value option
  | Val_builtin of string

and env = (string, value, Base.String.comparator_witness) Base.Map.t

val print_value : value -> unit
val print_error : error -> unit
val run_interpreter : let_binding list -> ((ident * value) list, error) result
