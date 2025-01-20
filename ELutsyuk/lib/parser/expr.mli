(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast

val pexpr_var : expr t
val pexpr_const : expr t
val pexpr_list : expr t -> expr t
val pexpr_tuple : expr t -> expr t

(* val pexpr_let : expr t -> expr t *)
val pexpr_app : expr t -> expr t
val pexpr_branch : expr t -> expr t
val pexpr_fun : pat t -> expr t -> expr t
val pexpr_match : pat t -> expr t -> expr t
val pexpr_let : expr t -> expr t
val plet_binding : expr t -> (rec_state * pat * expr) t
val pexpr_unary : expr t -> expr t
val pbin_op : expr t -> binop t -> expr t
val pexpr : expr t
