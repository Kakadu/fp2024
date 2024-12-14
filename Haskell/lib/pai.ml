(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

let parse_and_infer text print =
  match Parser.parse_line text with
  | Result.Ok list ->
    (match Inferencer.w_program list with
     | Result.Ok env -> Format.printf "%a" Inferencer.pp_typeenv env
     | Result.Error err -> Format.printf "%a" Pprint.pp_error err);
    if print then Parser.parse_and_print_line text
  | Result.Error error -> Format.printf "%s" error
;;
