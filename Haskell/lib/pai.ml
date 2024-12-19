(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

let parse_and_infer =
  let rec helper st names text print env =
    match text with
    | [] ->
      if env != Inferencer.typeenv_empty && env != Inferencer.typeenv_print_int
      then Format.printf "%a\n%!" Inferencer.pp_some_typeenv (names, env)
    | "" :: rest -> helper st names rest print env
    | line :: rest ->
      if print then Parser.parse_and_print_line line;
      (match Parser.parse_line line with
       | Result.Ok list ->
         (match Inferencer.w list env st with
          | st, Result.Ok (env, nn) ->
            helper st (List.fold_left (fun nn n -> n :: nn) names nn) rest print env
          | st, Result.Error err ->
            Format.printf "%a\n%!" Pprint.pp_error err;
            helper st names rest print env)
       | Result.Error error -> Format.printf "%s\n%!" error)
  in
  helper 0 []
;;

let parse_and_infer_line line env st =
  match Parser.parse_line line with
  | Result.Ok list ->
    (match Inferencer.w list env st with
     | st, Result.Ok (env, names) ->
       Format.printf "%a\n%!" Inferencer.pp_some_typeenv (names, env);
       env, st
     | st, Result.Error err ->
       Format.printf "%a\n%!" Pprint.pp_error err;
       env, st)
  | Result.Error error ->
    Format.printf "%s\n%!" error;
    env, st
;;

