(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

let parse_and_infer =
  let rec helper st names text dump_parsetree print_types env enviroment n =
    match text with
    | [] ->
      if env != Inferencer.typeenv_empty && env != Inferencer.initial_env && print_types
      then Format.printf "%a\n%!" Inferencer.pp_some_typeenv (names, env)
    | "" :: rest -> helper st names rest dump_parsetree print_types env enviroment n
    | line :: rest ->
      if dump_parsetree then Parser.parse_and_print_line line;
      (match Parser.parse_line line with
       | Result.Ok list ->
         (match Inferencer.w list env st with
          | st, Result.Ok (env, nn) ->
            (match Interpreter.eval list 0 0 with
             | Result.Ok (enviroment, n) ->
               helper
                 st
                 (List.fold_left (fun nn n -> n :: nn) names nn)
                 rest
                 dump_parsetree
                 print_types
                 env
                 enviroment
                 n
             | Result.Error (err, enviroment, n) ->
               Format.printf "%a\n%!" Pprint.pp_error err;
               helper
                 st
                 (List.fold_left (fun nn n -> n :: nn) names nn)
                 rest
                 dump_parsetree
                 print_types
                 env
                 enviroment
                 n)
          | st, Result.Error err ->
            Format.printf "%a\n%!" Pprint.pp_error err;
            helper st names rest dump_parsetree print_types env enviroment n)
       | Result.Error error -> Format.printf "%s\n%!" error)
  in
  helper 2 []
;;

let parse_and_infer_line line env st dump_parsetree print_types enviroment n =
  match Parser.parse_line line with
  | Result.Ok list ->
    if dump_parsetree then Parser.parse_and_print_line line;
    (match Inferencer.w list env st with
     | st, Result.Ok (env, names) ->
       if print_types then Format.printf "%a\n%!" Inferencer.pp_some_typeenv (names, env);
       (match Interpreter.eval list 0 0 with
        | Result.Ok (enviroment, n) -> env, st, enviroment, n
        | Result.Error (err, enviroment, n) ->
          Format.printf "%a\n%!" Pprint.pp_error err;
          env, st, enviroment, n)
     | st, Result.Error err ->
       Format.printf "%a\n%!" Pprint.pp_error err;
       env, st, enviroment, n)
  | Result.Error error ->
    Format.printf "%s\n%!" error;
    env, st, enviroment, n
;;
