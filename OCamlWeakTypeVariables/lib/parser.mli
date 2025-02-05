[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

val parse_expr : string -> (expression, string) result
val parse : string -> (structure_item, string) result
val parse_program : string -> (program, string) result
