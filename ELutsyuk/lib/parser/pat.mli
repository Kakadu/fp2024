(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast

val ppat_var : pat t
val ppat_const : pat t
val ppat_any : pat t
val ppat_tuple : pat t -> pat t
val ppat : pat t
