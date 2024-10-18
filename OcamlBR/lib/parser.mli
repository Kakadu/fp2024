(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val pexpr : Ast.expr Angstrom.t
val parse_expr : string -> (Ast.expr, string) result
