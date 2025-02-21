(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module ValueEnv : sig
  type t
  type value

  val default : t
  val pp_value : Format.formatter -> value -> unit
end

type error =
  [ `Division_by_zero
  | `Match_failure
  | `Type_mismatch
  | `Unbound_variable of string
  | `Args_after_not_variable_let
  | `Bound_several_times
  ]

type global_error =
  [ error
  | Inferencer.error
  ]

val pp_global_error : Format.formatter -> global_error -> unit

val run_interpreter
  :  Inferencer.TypeEnv.t
  -> ValueEnv.t
  -> int
  -> construction
  -> int
     * ( Inferencer.TypeEnv.t
         * ValueEnv.t
         * ( string
             , TypedTree.typ * ValueEnv.value
             , Base.String.comparator_witness )
             Base.Map.t
         , global_error )
         result
