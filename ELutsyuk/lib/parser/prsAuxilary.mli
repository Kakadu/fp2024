(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Forest.Ast

val skip_ws : unit t
val trim : 'a t -> 'a t
val token : string -> string t
val round_par : 'a t -> 'a t
val square_par : 'a t -> 'a t
val prs_id : id t
