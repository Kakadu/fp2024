(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

val pp_typ : Format.formatter -> TypesTree.typ -> unit
val pp_error : Format.formatter -> TypesTree.error -> unit
