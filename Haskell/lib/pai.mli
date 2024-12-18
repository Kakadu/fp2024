(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

val parse_and_infer : string list -> bool -> Inferencer.typeenv -> int -> unit
