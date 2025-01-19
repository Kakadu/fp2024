(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Interpreter

val parse_and_infer
  :  string list
  -> bool
  -> bool
  -> Inferencer.typeenv
  -> enviroment
  -> int
  -> unit

val parse_and_infer_line
  :  string
  -> Inferencer.typeenv
  -> int
  -> bool
  -> bool
  -> enviroment
  -> int
  -> Inferencer.typeenv * int * int * int
