(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `Impossible_error
  | `No_variable_rec
  | `No_arg_rec
  | `Bound_several_times
  | `Occurs_check of string * Ast.core_type
  | `No_variable of string
  | `Unification_failed of Ast.core_type * Ast.core_type
  ]

val pp_error : Format.formatter -> error -> unit

module VarSet : sig
  type t = Set.Make(String).t
end

type scheme = Scheme of VarSet.t * Ast.core_type

module TypeEnv : sig
  type t = (Ast.ident, scheme, Base.String.comparator_witness) Base.Map.t
end

val empty_env : TypeEnv.t
val env_with_print_int : TypeEnv.t
val run_inferencer : Ast.structure_item list -> TypeEnv.t -> (TypeEnv.t, error) result
