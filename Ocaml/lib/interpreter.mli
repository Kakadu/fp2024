(** Copyright 2021-2023, Daniil Kadochnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `Division_by_zero
  | `Unexpected_error
  | `Unfound_var of string
  ]

val pp_error : Format.formatter -> error -> unit

module R : sig
  type 'a t = ('a, error) result

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
end

module Interpret : sig
  module Env : sig
    type key = string
    type +!'a t
  end

  type value =
    | VConst of Ast.expr
    | VClosure of string option * value Env.t * Ast.expr
    | VRecClosure of string option * value Env.t * string list * Ast.expr
    | VDeclaration of (string * Ast.expr) list
end

val interpret : Ast.expr -> Interpret.value R.t
