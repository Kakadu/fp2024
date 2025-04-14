(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Forest.Ast

val prs_pat_var : pat t
val prs_pat_constant : pat t
val prs_pat_any : pat t
val prs_pat_tuple : pat t -> pat t
val prs_pat_cons : pat t -> pat t
val prs_pat_list : pat t -> pat t
val prs_pat : pat t
