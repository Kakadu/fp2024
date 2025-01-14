(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

val sep_by_comma : 'a list -> ('a -> string) -> string

val print_type : Ast.type' -> string
