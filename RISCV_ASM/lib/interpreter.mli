(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
module StringMap : Map.S with type key = string

type state =
  { registers : int64 StringMap.t
  ; pc : int64
  }

module ErrorMonad : sig
  type 'a t = ('a, string) result

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : string -> 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

val interpret : state -> ast -> state ErrorMonad.t
val show_state : state -> string
