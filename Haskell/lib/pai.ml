(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

let parse_and_infer =
  let rec helper st names text dump_parsetree print_types env =
    match text with
    | [] ->
      if env != Inferencer.typeenv_empty && env != Inferencer.initial_env && print_types
      then Format.printf "%a\n%!" Inferencer.pp_some_typeenv (names, env)
    | "" :: rest -> helper st names rest dump_parsetree print_types env
    | line :: rest ->
      if dump_parsetree then Parser.parse_and_print_line line;
      (match Parser.parse_line line with
       | Result.Ok list ->
         (match Inferencer.w list env st with
          | st, Result.Ok (env, nn) ->
            (match Interpreter.eval list enviroment 0 with
             | Result.Ok (enviroment, n) -> ()
             | Result.Error (err, enviroment, n) ->
               Format.printf "%a\n%!" Pprint.pp_error err);
            helper
              st
              (List.fold_left (fun nn n -> n :: nn) names nn)
              rest
              dump_parsetree
              print_types
              env
          | st, Result.Error err ->
            Format.printf "%a\n%!" Pprint.pp_error err;
            helper st names rest dump_parsetree print_types env)
       | Result.Error error -> Format.printf "%s\n%!" error)
  in
  helper 2 []
;;

let parse_and_infer_line line env st dump_parsetree print_types =
  match Parser.parse_line line with
  | Result.Ok list ->
    if dump_parsetree then Parser.parse_and_print_line line;
    (match Inferencer.w list env st with
     | st, Result.Ok (env, names) ->
       if print_types then Format.printf "%a\n%!" Inferencer.pp_some_typeenv (names, env);
       (match Interpreter.eval list enviroment 0 with
        | Result.Ok (enviroment, n) -> ()
        | Result.Error (err, enviroment, n) -> Format.printf "%a\n%!" Pprint.pp_error err);
       env, st
     | st, Result.Error err ->
       Format.printf "%a\n%!" Pprint.pp_error err;
       env, st)
  | Result.Error error ->
    Format.printf "%s\n%!" error;
    env, st
;;
