(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

val parse_line : string -> Ast.binding
val parse_and_print_line : string -> unit
