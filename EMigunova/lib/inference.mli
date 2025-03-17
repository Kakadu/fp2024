(** Copyright 2024, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

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
  | `Occurs_check of string * ttype
    (** Represents an occurs check failure.
        This occurs when attempting to unify types, and one type is found to occur within another in a way that violates the rules of type systems.
        E.g. [let rec f x = f] *)
  | `No_variable of string
    (** Represents an error indicating that a variable could not be found in the current scope. *)
  | `Unification_failed of string * ttype * ttype
    (** Represents that type unification has failed.
        This occurs when two types cannot made equivalent during type inference. *)
  ]

(*val pp_error : Format.formatter -> error -> unit*)

module VarSet : sig
  type t = Set.Make(String).t
end

type scheme = Scheme of VarSet.t * ttype

module TypeEnv : sig
  type t = (ident, scheme, Base.String.comparator_witness) Base.Map.t
end

(*val empty_env : TypeEnv.t*)
val env_with_print_funs : TypeEnv.t

(*type structure = let_binding list*)

val run_inferencer : structure_item list -> ((ident option * ttype) list, error) result

(*for debug*)
val infer : ident -> unit list
val run_infer : (ident -> 'a) -> 'a list
