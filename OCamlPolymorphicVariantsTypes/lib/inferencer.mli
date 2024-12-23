(** Copyright 2024-2027, Ilia Suponev, Dmitri Chirkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree
open Ast
open Stdlib.Format

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of core_type * core_type
  | `Not_implemented
  ]

module TypeEnv : sig
  val pp_env : Format.formatter -> (string, scheme, 'a) Base.Map.t -> unit
end

val run_infer
  :  struct_item list
  -> ((string, scheme, Base.String.comparator_witness) Base.Map.t, error) result

val pp_error : formatter -> error -> unit
