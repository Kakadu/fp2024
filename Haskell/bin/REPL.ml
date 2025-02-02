(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Haskell_lib

type opts =
  { mutable dump_parsetree : bool
  ; mutable print_types : bool
  ; mutable read_from_file : string
  }

let () =
  let opts = { dump_parsetree = false; print_types = false; read_from_file = "" } in
  let _ =
    let open Stdlib.Arg in
    parse
      [ "-dparsetree", Unit (fun () -> opts.dump_parsetree <- true), "Dump parse tree"
      ; "-ptypes", Unit (fun () -> opts.print_types <- true), "Print types"
      ]
      (fun file ->
        if Sys.file_exists file
        then opts.read_from_file <- file
        else (
          Stdlib.Format.eprintf "File doesn't exist\n";
          Stdlib.exit 1))
      "Parse and print ast and types"
  in
  let is_stdin =
    match opts.read_from_file with
    | "" -> true
    | _ -> false
  in
  if not is_stdin
  then
    Interpreter.interpret
      opts.dump_parsetree
      opts.print_types
      (String.split_on_char
         '\n'
         (In_channel.with_open_text opts.read_from_file In_channel.input_all))
      Inferencer.initial_env
      Eval.init_env
      Eval.init_fresh
  else (
    let rec helper inf_env st eval_env fresh =
      let line =
        try input_line stdin with
        | End_of_file -> ":quit"
      in
      match line with
      | ":quit" -> ()
      | "" -> helper inf_env st eval_env fresh
      | _ ->
        let inf_env, st, eval_env, fresh =
          Interpreter.interpret_line
            line
            inf_env
            st
            opts.dump_parsetree
            opts.print_types
            eval_env
            fresh
        in
        helper inf_env st eval_env fresh
    in
    helper Inferencer.initial_env 2 Eval.init_env Eval.init_fresh)
;;
