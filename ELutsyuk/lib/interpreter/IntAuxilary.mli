(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest.ValuesTree

module Res : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> ('a, error) Result.t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module EvalEnv : sig
  type t = env

  val empty : t
  val extend : t -> string -> value -> t
  val compose : t -> t -> t
  val find_val : t -> string -> value Res.t
end
