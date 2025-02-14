(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `No_variable_rec
    (** Represents an error where a recursive variable is not allowed because that would lead to infinite recursion.
        E.g. [let rec x = x + 1] *)
  | `No_arg_rec
    (** Represents an error where the left-hand side of the recursive binding is not a var.
        E.g. [let rec [ a; b ] = ..] *)
  | `Bound_several_times of string
    (** Represents an error where a pattern bound the variable multiple times.
        E.g. [let x, x = ..] *)
  | `Occurs_check of string * Ast.core_type
    (** Represents an occurs check failure.
        This occurs when attempting to unify types, and one type is found to occur within another in a way that violates the rules of type systems.
        E.g. [let rec f x = f] *)
  | `No_variable of string
    (** Represents an error indicating that a variable could not be found in the current scope. *)
  | `Unification_failed of Ast.core_type * Ast.core_type
    (** Represents that type unification has failed.
        This occurs when two types cannot made equivalent during type inference. *)
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
val env_with_print_funs : TypeEnv.t

val run_inferencer
  :  ?debug:bool
  -> TypeEnv.t
  -> Ast.structure
  -> (TypeEnv.t * (Ast.ident option * Ast.core_type) list, error) result
