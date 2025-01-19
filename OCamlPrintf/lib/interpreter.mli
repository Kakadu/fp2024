(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `Type_error
  | `Division_by_zero
  | `Match_failure
  | `No_variable of string
  ]

val pp_error : Format.formatter -> error -> unit

type value =
  | Val_integer of int
  | Val_char of char
  | Val_string of string
  | Val_fun of Ast.rec_flag * Ast.pattern * Ast.pattern list * Ast.Expression.t * env
  | Val_function of Ast.Expression.t Ast.case list * env
  | Val_tuple of value * value * value list
  | Val_construct of string * value option
  | Val_builtin of string

and env = (string, value, Base.String.comparator_witness) Base.Map.t

val pp_value : Format.formatter -> value -> unit
val empty_env : env
val env_with_print_funs : env

val run_interpreter
  :  env
  -> Ast.structure
  -> (env * (Ast.ident option * value) list, error) result
