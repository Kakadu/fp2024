(** Copyright 2024, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

val parse_id : ident t
val parse_int : constant t
val parse_char : constant t
val parse_str : constant t
val parse_bool : constant t
val parse_unit : constant t
val parse_const : constant t
val parse_any_pattern : pattern t
val parse_var_pattern : pattern t
val parse_const_pattern : pattern t
val parse_tuple_pattern : pattern t -> pattern t
val parse_pattern : pattern t
val parse_expr_var : expression t
val parse_expr_const : expression t
val parse_expr_tuple : expression t -> expression t
val parse_expr_list : expression t -> expression t
val parse_expr_base_elements : expression t
val parse_expression : expression t
