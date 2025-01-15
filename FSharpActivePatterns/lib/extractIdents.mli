(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module type R = sig
  type 'a t
  type error

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val bound_error : error

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module Make (R : R) : sig
  type t

  val extract_names_from_pattern : pattern -> t R.t
  val extract_names_from_patterns : pattern list -> t R.t
  val extract_bind_names_from_let_binds : let_bind list -> t R.t
  val extract_bind_patterns_from_let_binds : let_bind list -> pattern list
  val elements : t -> string list
end
