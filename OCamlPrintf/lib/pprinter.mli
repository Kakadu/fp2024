(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val pp_rec_flag : Format.formatter -> Ast.rec_flag -> unit
val pp_ident : Format.formatter -> string -> unit
val pp_constant : Format.formatter -> Ast.constant -> unit
val pp_core_type : Format.formatter -> Ast.core_type -> unit
val pp_pattern : Format.formatter -> Ast.pattern -> unit
val pp_expression : Format.formatter -> Ast.Expression.t -> unit
val pp_value_binding : Format.formatter -> Ast.Expression.value_binding_exp -> unit
val pp_case : Format.formatter -> Ast.Expression.case_exp -> unit
val pp_structure_item : Format.formatter -> Ast.structure_item -> unit
val pp_structure : Format.formatter -> Ast.structure -> unit
