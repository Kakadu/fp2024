(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Inferencer
open Typedtree

type enviroment

let eval (binding : Ast.binding list) (env : enviroment) (n : int) =
  Result.Ok (env, 0)
;;
