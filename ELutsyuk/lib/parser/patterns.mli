(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast

val prs_pat_var : pat t
val prs_pat_const : pat t
val prs_pat_any : pat t
val prs_pat_tuple : pat t -> pat t
val prs_pat : pat t
