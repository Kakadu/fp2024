(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

val parse_and_infer : string list -> bool -> bool -> Inferencer.typeenv -> unit

val parse_and_infer_line
  :  string
  -> Inferencer.typeenv
  -> int
  -> bool
  -> bool
  -> Inferencer.typeenv * int
