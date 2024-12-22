(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

val type_check : Ast.file -> (unit, Errors.error) result
