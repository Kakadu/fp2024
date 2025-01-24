(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast

val trim : 'a t -> 'a t
val token : id -> id t
val round_par : 'a t -> 'a t
val square_par : 'a t -> 'a t
val pid : id t
val pint : const t
val pstr : const t
val pbool : const t
val punit : const t
val pconst : const t
