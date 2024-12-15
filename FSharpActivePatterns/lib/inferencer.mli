(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open TypedTree
open Format

module TypeEnvironment : sig
  type t

  val empty : t
end

type error =
  [ `Occurs_check
  | `Undef_var of string
  | `Unification_failed of typ * typ
  | `WIP of string
  ]

val pp_error : formatter -> error -> unit

val infer
  :  construction
  -> TypeEnvironment.t
  -> (TypeEnvironment.t * typ list, error) result
