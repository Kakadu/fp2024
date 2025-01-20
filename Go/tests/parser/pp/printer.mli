(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

val print_type : type' -> string
val print_ident : ident -> string
val print_expr : expr -> string
val print_stmt : stmt -> string
val print_file : file -> string
