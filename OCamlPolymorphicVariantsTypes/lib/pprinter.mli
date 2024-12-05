(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser_utility
open Format

val pp_identifier : formatter -> identifier -> unit
val pp_literal : formatter -> literal -> unit
val pp_unary_operator : formatter -> unary_operator -> unit
val pp_binary_operator : formatter -> binary_operator -> unit
val pp_recursive_type : formatter -> recursive_type -> unit
val pp_core_type : formatter -> core_type -> unit
val pp_pattern : formatter -> pattern -> unit
val pp_case : formatter -> case -> unit
val pp_expression : formatter -> expression -> unit
val pp_value_binding : formatter -> value_binding -> unit
val pp_definition : formatter -> definition -> unit
val pp_struct_item : formatter -> struct_item -> unit
val pp_program : formatter -> program -> unit
val pp_parse_result : formatter -> (formatter -> 'a -> unit) -> 'a parse_result -> unit
