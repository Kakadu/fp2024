(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser_utility

val ident : string parser
val integer : literal parser
val boolean : literal parser
val pattern_parser : pattern parser
val pvariable : pattern parser
val ptuple : pattern parser
val const_expr : expression parser
val variable : expression parser
val basic_expr : expression parser
val unary_expr : expression parser
val bracket_expr : expression parser
val summary_expr : expression parser
val multiply_expr : expression parser
val compare_expr : expression parser
val boolean_expr : expression parser
val if_expr : expression parser
val apply_expr : expression parser
val lambda_expr : expression parser
