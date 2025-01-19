(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `Type_error
    (** Represents a type error that occurs when a type mismatch is detected in an expression. *)
  | `Division_by_zero
    (** Represents the error that occurs when attempting to perform a division by zero operation. *)
  | `Match_failure
    (** Represents a match error occurs when a pattern matching attempt fails. *)
  | `No_variable of Ast.ident
    (** Represents an error that occurs when attempting to use a variable that has not been declared or initialized. *)
  ]

val pp_error : Format.formatter -> error -> unit

type value =
  | Val_integer of int
  | Val_char of char
  | Val_string of string
  | Val_fun of Ast.rec_flag * Ast.pattern * Ast.pattern list * Ast.Expression.t * env
  | Val_function of Ast.Expression.t Ast.case list * env
  | Val_tuple of value * value * value list
  | Val_construct of Ast.ident * value option
  | Val_builtin of Ast.ident

and env = (Ast.ident, value, Base.String.comparator_witness) Base.Map.t

val pp_value : Format.formatter -> value -> unit
val empty_env : env
val env_with_print_funs : env

val run_interpreter
  :  env
  -> Ast.structure
  -> (env * (Ast.ident option * value) list, error) result
