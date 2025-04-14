(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Forest.TypesTree

val prs_typ_constant : typ t
val prs_typ_arrow : typ t -> typ t
val prs_typ_tup : typ t -> typ t
val prs_typ_list : typ t -> typ t
val prs_typ_option : typ t -> typ t
val prs_typ : typ t
