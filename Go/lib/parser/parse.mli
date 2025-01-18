(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Angstrom

val parse_ident : ident t
val parse_type : type' t
val parse_expr : expr t
val parse_stmt : stmt t
val parse_file : file t
val parse : 'a t -> string -> ('a, string) result
