(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

type env = (ident, value, String.comparator_witness) Map.t

and value =
  | ValueInt of int
  | ValueBool of bool
  | ValueString of string
  | ValueUnit
  | ValueClosure of is_rec * pattern * pattern list * expr * env
  | ValueTuple of value * value * value list
  | ValueList of value list
  | ValueOption of value option
  | ValueBuiltin of (value -> (value, value_error) Result.t)

and value_error =
  | UnboundVariable of ident
  | TypeError
  | DivisionByZeroError
  | PatternMatchingError
  | LHS

val pp_value_error : Stdlib.Format.formatter -> value_error -> unit

module Inter : sig
  val eval_structure : program -> (env, value_error) Result.t
end
