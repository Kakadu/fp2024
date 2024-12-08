(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

val parse_line : string -> (Ast.binding list, string) Result.t
val parse_and_print_line : string -> unit
val parse_expr : string -> (Ast.expr, string) Result.t
