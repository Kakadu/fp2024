(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Eval

val interpret
  :  string list
  -> bool
  -> bool
  -> Inferencer.typeenv
  -> enviroment
  -> int
  -> unit

val interpret_line
  :  string
  -> Inferencer.typeenv
  -> int
  -> bool
  -> bool
  -> enviroment
  -> int
  -> Inferencer.typeenv * int * int * int
