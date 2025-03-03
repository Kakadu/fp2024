(** Copyright 2024-2025, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
module StringMap : Map.S with type key = string (* used to organize string memory *)
module Int64Map : Map.S with type key = Int64.t (* used to organize int64 memory *)

type state =
  { program : ast
  ; registers : int64 StringMap.t
  ; vregisters : int64 array StringMap.t
  ; max_vector_length : int
  ; vector_element_length : int
  ; vector_length : int
  ; memory_int : int64 Int64Map.t
  ; memory_str : string Int64Map.t
  ; memory_writable : bool Int64Map.t
  ; program_idx : int64
  (* index of current expr in program, used as analogy of Program Counter, each expr step is 4 *)
  }

(* Combined monad for errors and state *)
module type COMBINED_MONAD = sig
  type ('s, 'a) t

  val return : 'a -> ('s, 'a) t
  val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  val fail : string -> ('s, 'a) t
  val read : ('s, 's) t
  val write : 's -> ('s, unit) t
  val run : ('s, 'a) t -> 's -> ('s * 'a, string) result

  module Syntax : sig
    val ( let* ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  end
end

module CombinedMonad : COMBINED_MONAD

val interpret : ast -> (state * state, label) result
val show_state : state -> string
