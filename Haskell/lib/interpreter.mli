(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Inferencer
open Typedtree

type enviroment = int

val eval
  :  Ast.binding list
  -> enviroment
  -> int
  -> (enviroment * int, error * enviroment * int) Result.t
