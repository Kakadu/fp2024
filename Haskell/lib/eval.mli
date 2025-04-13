(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Ast

type crit_err =
  [ `Typing_err
  | `Not_exh
  | `Div_by_zero
  | `Negative_exponent
  ]

type env
type fresh

val eval
  :  env
  -> fresh
  -> binding_list
  -> (env * fresh, crit_err * (env * fresh)) Result.t

val init_env : env
val init_fresh : fresh
