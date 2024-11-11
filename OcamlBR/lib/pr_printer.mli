(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val prpr_structure : Format.formatter -> Ast.structure_item list -> unit
val pp_structure_item : structure_item -> string
