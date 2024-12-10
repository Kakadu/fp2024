(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse : string -> (Ast.expr list, string) result
val parse_string_expr : string -> (Ast.expr, string) result
