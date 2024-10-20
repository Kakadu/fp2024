(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val parse : string -> (structure, string) result
val parse_to_string : string -> string
