(** Copyright 2024-2025, Ekaterina Tenyaeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | TypeError
    (** Represents a type error that occurs when a type mismatch is detected in an expression. *)
  | DivisionByZero
    (** Represents the error that occurs when attempting to perform a division by zero operation. *)
  | MatchFailure
    (** Represents a match error occurs when a pattern matching attempt fails. *)
  | NoVariable of Ast.id
    (** Represents an error that occurs when attempting to use a variable that has not been declared or initialized. *)

val pp_error : Format.formatter -> error -> unit

type value =
  | ValInt of int
  | ValChar of char
  | ValString of string
  | ValUnit
  | ValBool of bool
  | ValFun of Ast.rec_flag * Ast.pattern * Ast.expr * env
  | ValFunction of Ast.case_expr list * env
  | ValTuple of value * value * value list
  | ValList of value list
  | ValOption of value option
  | ValBuiltin of Ast.id

and env = (Ast.id, value, Base.String.comparator_witness) Base.Map.t

val pp_value : Format.formatter -> value -> unit
val empty_env : env
val env_with_print_funs : env

val run_interpreter
  :  env
  -> Ast.structure
  -> (env * (Ast.id option * value) list, error) result