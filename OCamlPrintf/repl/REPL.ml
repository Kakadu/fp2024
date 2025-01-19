(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib
open Stdio

type opts =
  { mutable dump_parsetree : bool
  ; mutable inference : bool
  ; mutable input_file : string option
  }

let pp_global_error ppf = function
  | #Inferencer.error as e -> Inferencer.pp_error ppf e
  | #Interpreter.error as e -> Interpreter.pp_error ppf e
;;

let run_single dump_parsetree inference input_source =
  let run text env_infer env_inter =
    let ast = Parser.parse text in
    match ast with
    | Error error ->
      print_endline error;
      env_infer, env_inter
    | Ok ast ->
      if dump_parsetree
      then (
        print_endline (Ast.show_structure ast);
        env_infer, env_inter)
      else (
        match Inferencer.run_inferencer env_infer ast with
        | Error e ->
          print_endline (Format.asprintf "Infer error: %a" pp_global_error e);
          env_infer, env_inter
        | Ok (env_infer, out_list) ->
          if inference
          then (
            List.iter
              (function
                | Some id, type' ->
                  print_endline
                    (Format.asprintf "val %s : %a" id Pprinter.pp_core_type type')
                | None, type' ->
                  print_endline (Format.asprintf "- : %a" Pprinter.pp_core_type type'))
              out_list;
            env_infer, env_inter)
          else (
            match Interpreter.run_interpreter env_inter ast with
            | Ok (env_inter, _) ->
              ();
              (* List.iter
                 (function
                 | Some id, val' ->
                 print_endline
                 (Format.asprintf "val %s = %a" id Interpreter.pp_value val')
                 | None, val' ->
                 print_endline (Format.asprintf "- = %a" Interpreter.pp_value val'))
                 out_list; *)
              env_infer, env_inter
            | Error e ->
              print_endline (Format.asprintf "Interpreter error: %a" pp_global_error e);
              env_infer, env_inter))
  in
  let env_infer, env_inter =
    Inferencer.env_with_print_funs, Interpreter.env_with_print_funs
  in
  match input_source with
  | Some file_name ->
    let text = In_channel.read_all file_name |> String.trim in
    let _, _ = run text env_infer env_inter in
    ()
  | None ->
    let rec input_lines lines env_infer env_inter =
      match In_channel.input_line stdin with
      | Some line ->
        if line = ";;" || String.ends_with ~suffix:";;" line
        then (
          let env_infer, env_inter = run (lines ^ line) env_infer env_inter in
          input_lines "" env_infer env_inter)
        else input_lines (lines ^ line) env_infer env_inter
      | None -> ()
    in
    let _ = input_lines "" env_infer env_inter in
    ()
;;

let () =
  let options = { dump_parsetree = false; inference = false; input_file = None } in
  let () =
    let open Arg in
    parse
      [ ( "-dparsetree"
        , Unit (fun () -> options.dump_parsetree <- true)
        , "Dump parse tree, don't evaluate anything" )
      ; ( "-inference"
        , Unit (fun () -> options.inference <- true)
        , "Inference, don't evaluate anything" )
      ; ( "-fromfile"
        , String (fun filename -> options.input_file <- Some filename)
        , "Read code from the file" )
      ]
      (fun _ ->
        Format.eprintf "Positional arguments are not supported\n";
        exit 1)
      "Read-Eval-Print-Loop for custom language"
  in
  run_single options.dump_parsetree options.inference options.input_file
;;
