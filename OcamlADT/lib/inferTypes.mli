(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Ast.TypeExpr
open Stdlib

type binder = int [@@deriving show]

module VarSet : sig
  include Set.S with type elt = string

  val pp : formatter -> t -> unit
end

type binder_set = VarSet.t [@@deriving show]
type scheme = Forall of binder_set * t [@@deriving show]

val binder_to_list : binder_set -> int list

val minimize
  :  int list
  -> (string, string, Base.String.comparator_witness) Base.Map.t * int * int

val pprint_type
  :  ?poly_names_map:(string, string, Base.String.comparator_witness) Base.Map.t
  -> formatter
  -> t
  -> unit

type error =
  | Occurs_check of string * t
  | Unification_failed of t * t
  | Unbound_variable of string
  | Arity_mismatch
  | Undeclared_type of string
  | Wrong_rec
  | Unsupported_operator of string
  | Incorrect_list_lengths

val pp_inf_err : formatter -> error -> unit
