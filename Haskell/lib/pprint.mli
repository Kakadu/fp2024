(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)
open Typedtree

val pp_ty : Format.formatter -> ty -> unit
val pp_error : Format.formatter -> error -> unit
