(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

let rec parse_and_infer text print env st =
  match text with
  | [] ->
    if env != Inferencer.typeenv_empty && env != Inferencer.typeenv_print_int
    then Format.printf "%a\n%!" Inferencer.pp_typeenv env
  | "" :: rest -> parse_and_infer rest print env st
  | line :: rest ->
    if print then Parser.parse_and_print_line line;
    (match Parser.parse_line line with
     | Result.Ok list ->
       (match Inferencer.w list env st with
        | st, Result.Ok env -> parse_and_infer rest print env st
        | st, Result.Error err ->
          Format.printf "%a\n%!" Pprint.pp_error err;
          parse_and_infer rest print env st)
     | Result.Error error -> Format.printf "%s\n%!" error)
;;

let parse_and_infer_line line env st =
  match Parser.parse_line line with
  | Result.Ok list ->
    (match Inferencer.w list env st with
     | st, Result.Ok env ->
       Format.printf "%a\n%!" Inferencer.pp_typeenv env;
       env, st
     | st, Result.Error err ->
       Format.printf "%a\n%!" Pprint.pp_error err;
       env, st)
  | Result.Error error ->
    Format.printf "%s\n%!" error;
    env, st
;;
