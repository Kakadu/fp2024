(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

let parse_and_infer line =
  match Parser.parse_line line with
  | Result.Ok list ->
    (match Inferencer.w list Inferencer.initial_env 2 with
     | _, Result.Ok (env, names) ->
       Format.printf "%a\n%!" Inferencer.pp_some_typeenv (names, env)
     | _, Result.Error err -> Format.printf "%a\n%!" Pprint.pp_error err)
  | Result.Error error -> Format.printf "%s\n%!" error
;;
