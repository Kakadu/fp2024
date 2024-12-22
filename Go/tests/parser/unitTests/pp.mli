(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

val pp : ('a -> string) -> 'a Angstrom.t -> string -> unit
