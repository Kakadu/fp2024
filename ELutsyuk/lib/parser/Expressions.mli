(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Forest.Ast

val prs_expr_var : expr t
val prs_expr_const : expr t
val prs_expr_list : expr t -> expr t
val prs_expr_tuple : expr t -> expr t
val prs_expr_app : expr t -> expr t
val prs_expr_branch : expr t -> expr t
val prs_expr_fun : expr t -> expr t
val prs_expr_let : expr t -> expr t
val prs_let_binding : expr t -> let_binding t

(* val prs_expr_unary : expr t -> expr t *)
val prs_bin_op : binop -> id -> (expr -> expr -> expr) t
val prs_option : expr t -> expr t
val prs_expr_type : expr t -> expr t
val prs_expr : expr t
