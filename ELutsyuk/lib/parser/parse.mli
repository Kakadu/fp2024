(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Ast

val parse : string -> (program, string) result
val parse_to_string : string -> string
