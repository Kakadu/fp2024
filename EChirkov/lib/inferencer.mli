(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Stdlib

module VarSet : sig
  include Set.S with type elt = int
end

module Scheme : sig
  type binder_set = VarSet.t
  type t = S of binder_set * core_type
end

type error =
  | OccursCheck of binder * core_type
  | NoVariable of id
  | UnificationFailed of core_type * core_type
  | InvalidLeftHandSide
  | InvalidRightHandSide

val inference
  :  structure_item list
  -> ((string, Scheme.t, Base.String.comparator_witness) Base.Map.t, error) result

val pp_error : Format.formatter -> error -> unit
val print_env : (string, Scheme.t, 'a) Base.Map.t -> unit
