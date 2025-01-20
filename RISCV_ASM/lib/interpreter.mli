(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
module StringMap : Map.S with type key = string
module Int64Map : Map.S with type key = Int64.t

type state =
  { registers : int64 StringMap.t
  ; vector_registers : int64 array StringMap.t
  ; max_vector_length : int
  ; vector_element_length : int
  ; vector_length : int
  ; memory_int : int64 Int64Map.t
  ; memory_str : string Int64Map.t
  ; memory_writable : bool Int64Map.t
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
