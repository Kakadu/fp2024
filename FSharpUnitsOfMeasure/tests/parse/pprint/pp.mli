(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Base

val pp : ('a -> string) -> 'a Angstrom.t -> string -> unit

val pp2 : (Format.formatter -> 'a -> unit) -> 'a Angstrom.t -> string -> unit
