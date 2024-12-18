(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

let rec parse_and_infer text print env =
  match text with
  | [] -> ()
  | "" :: rest -> parse_and_infer rest print env
  | line :: rest ->
    (match Parser.parse_line line with
     | Result.Ok list ->
       (match Inferencer.w_program list env with
        | Result.Ok env ->
          Format.printf "%a\n" Inferencer.pp_typeenv env;
          parse_and_infer rest print env
        | Result.Error err ->
          Format.printf "%a\n" Pprint.pp_error err;
          parse_and_infer rest print env);
       if print then Parser.parse_and_print_line line
     | Result.Error error -> Format.printf "%s\n" error)
;;
