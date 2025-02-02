(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

let interpret dump_parsetree print_types =
  let rec helper st names text inf_env eval_env fresh =
    match text with
    | [] ->
      if inf_env != Inferencer.typeenv_empty
         && inf_env != Inferencer.initial_env
         && print_types
      then Format.printf "%a\n%!" Inferencer.pp_some_typeenv (names, inf_env)
    | "" :: rest -> helper st names rest inf_env eval_env fresh
    | line :: rest ->
      if dump_parsetree then Parser.parse_and_print_line line;
      (match Parser.parse_line line with
       | Result.Ok bnds ->
         (match Inferencer.w bnds inf_env st with
          | st, Result.Ok (inf_env', nn) ->
            let eval_env', fresh' =
              match Eval.eval eval_env fresh bnds with
              | Result.Ok (eval_env', fresh') -> eval_env', fresh'
              | Result.Error (err, (eval_env', fresh')) ->
                Format.printf "%a\n%!" Pprint.pp_eval_err err;
                eval_env', fresh'
            in
            helper
              st
              (List.fold_left (fun nn n -> n :: nn) names nn)
              rest
              inf_env'
              eval_env'
              fresh'
          | st, Result.Error err ->
            Format.printf "%a\n%!" Pprint.pp_error err;
            helper st names rest inf_env eval_env fresh)
       | Result.Error error -> Format.printf "%s\n%!" error)
  in
  helper 2 []
;;

let interpret_line line inf_env st dump_parsetree print_types eval_env fresh =
  match Parser.parse_line line with
  | Result.Ok bnds ->
    if dump_parsetree then Parser.parse_and_print_line line;
    (match Inferencer.w bnds inf_env st with
     | st, Result.Ok (inf_env', names) ->
       if print_types
       then Format.printf "%a\n%!" Inferencer.pp_some_typeenv (names, inf_env');
       (match Eval.eval eval_env fresh bnds with
        | Result.Ok (eval_env', fresh') -> inf_env', st, eval_env', fresh'
        | Result.Error (err, (eval_env', fresh')) ->
          Format.printf "%a\n%!" Pprint.pp_eval_err err;
          inf_env', st, eval_env', fresh')
     | st, Result.Error err ->
       Format.printf "%a\n%!" Pprint.pp_error err;
       inf_env, st, eval_env, fresh)
  | Result.Error error ->
    Format.printf "%s\n%!" error;
    inf_env, st, eval_env, fresh
;;
