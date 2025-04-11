(** Copyright 2025, Migunova Anastasia *)

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
val parse_expr_list_sugar : expression t -> expression t
val parse_expr_base_elements : expression t -> expression t
val parse_bin_op_expression : expression t -> expression t
val parse_if_when_else : expression t -> expression t
val parse_match_with : expression t -> expression t
val parse_let_biding : expression t -> let_binding t
val parse_in_construction : expression t -> expression t
val parse_anonymouse_fun : expression t -> expression t
val parse_function_fun : expression t -> expression t
val parse_expression : expression t
val parse : ident -> (let_binding list, ident) result
