(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast

val prs_expr_var : expr t
val prs_expr_const : expr t
val prs_expr_list : expr t -> expr t
val prs_expr_tuple : expr t -> expr t
val prs_expr_app : expr t -> expr t
val prs_expr_branch : expr t -> expr t
val prs_expr_fun : pat t -> expr t -> expr t

(* val pexpr_match : pat t -> expr t -> expr t *)
val prs_expr_let : expr t -> expr t
val prs_let_binding : expr t -> let_binding t
val prs_expr_unary : expr t -> expr t
val prs_bin_op : expr t -> binop t -> expr t
val prs_option : expr t -> expr t
val prs_expr : expr t
