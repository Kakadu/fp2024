(** Copyright 2024-2025, Sultanov Muhammet and Kudrya Alexander *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse : string -> (Ast.structure_item list, string) result
val parse_prefix : string -> (Ast.structure_item list, string) result
