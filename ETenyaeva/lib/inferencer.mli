(** Copyright 2024-2025, Ekaterina Tenyaeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | NoVariableRec
  | NoArgRec
  | SeveralBounds of string
  | OccursCheck of string * Ast.typ
  | NoVariable of string
  | UnificationFailed of Ast.typ * Ast.typ

val pp_type : Format.formatter -> Ast.typ -> unit
val pp_error : Format.formatter -> error -> unit

module VarSet : sig
  type t
  val empty : t
  val add : string -> t -> t
  val remove : string -> t -> t
  val mem : string -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val elements : t -> string list
end

type scheme = Scheme of VarSet.t * Ast.typ

module TypeEnv : sig
  type t = (Ast.id, scheme, Base.String.comparator_witness) Base.Map.t
end
val empty_env : TypeEnv.t

val run_inferencer
  :  TypeEnv.t
  -> Ast.structure
  -> (TypeEnv.t * (Ast.id option * Ast.typ) list, error) result
