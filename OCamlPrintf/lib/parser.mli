(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

val skip_whitespaces : unit t
val parse_comments : unit t
val ws : unit t
val skip_parens : 'a t -> 'a t
val is_keyword : string -> bool
val parse_ident : string t
val parse_rec_flag : rec_flag t
val parse_const_int : constant t
val parse_const_char : constant t
val parse_const_string : constant t
val parse_constant : constant t
val parse_pat_any : pattern t
val parse_pat_var : pattern t
val parse_pat_constant : pattern t
val parse_pat_tuple : pattern t -> pattern t
val parse_pat_construct : pattern t -> pattern t
val parse_pattern : pattern t
val parse_chain_left_associative : 'a t -> ('a -> 'a -> 'a) t -> 'a t

val bin_op
  :  ('a -> (expression -> expression -> expression) t -> 'b)
  -> 'a
  -> expression t
  -> 'b

val parse_left_bin_op : expression t -> expression t -> expression t
val select_operator : string list -> expression t
val mul_div : expression t
val add_sub : expression t
val cmp : expression t
val parse_operator : expression t -> expression t
val parse_exp_ident : expression t
val parse_exp_constant : expression t
val parse_cases : expression t -> case list t
val parse_exp_match : expression t -> expression t
val parse_exp_tuple : expression t -> expression t
val parse_exp_construct : expression t -> expression t
val parse_exp_ifthenelse : expression t -> expression t
val parse_expression : expression t
val parse_fun_binding : value_binding t
val parse_simple_binding : value_binding t
val parse_value_binding_list : value_binding list t
val parse_struct_value : structure_item t
val parse_structure : structure_item list t
val parse : string -> structure_item list option
