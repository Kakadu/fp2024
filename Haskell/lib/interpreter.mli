(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

val interpret
  :  string list
  -> bool
  -> bool
  -> Inferencer.typeenv
  -> Eval.env
  -> Eval.fresh
  -> unit

val interpret_line
  :  string
  -> Inferencer.typeenv
  -> int
  -> bool
  -> bool
  -> Eval.env
  -> Eval.fresh
  -> Inferencer.typeenv * int * Eval.env * Eval.fresh
