(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

module IntSet : sig
  type t = Stdlib.Set.Make(Int).t
end

type error =
  | OccursCheck of int * ty
  | NoVariable of string
  | UnificationFailed of ty * ty
  | SeveralBounds of string
  | NotImplement

val pp_error : Stdlib.Format.formatter -> error -> unit

module Scheme : sig
  type t = S of IntSet.t * ty
end

module TypeEnv : sig
  type t = (ident, Scheme.t, String.comparator_witness) Map.t
end

val run_infer : Ast.program -> (TypeEnv.t, error) Result.t
