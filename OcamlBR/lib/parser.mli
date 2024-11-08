(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val parse_expr : string -> (structure, string) result
val xd : string -> unit
val pp_structure : Format.formatter -> Ast.structure_item list -> unit
val pp_structure_item : structure_item -> string
