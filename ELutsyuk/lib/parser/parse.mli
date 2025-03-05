(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest.Ast

val parse : string -> (program, string) result
val parse_program : string -> unit
(* val parse_to_string : string -> string *)
