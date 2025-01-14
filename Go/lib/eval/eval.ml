(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open EvalMonad
open EvalMonad.Monad

let init_state = { global_env = MapIdent.empty; running = None; sleeping = [] }

let eval =
  run (return ()) init_state
  |> function
  | _, res -> res (* mb check final state *)
;;
