(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

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
    Haskell_lib.Interpreter.interpret
      (String.split_on_char
         '\n'
         (In_channel.with_open_text opts.read_from_file In_channel.input_all))
      opts.dump_parsetree
      opts.print_types
      Haskell_lib.Inferencer.initial_env
      0
      0
  else (
    let rec helper (env, st, enviroment, n) =
      (* TODO(Kakadu): Why curry? *)
      let line =
        try input_line stdin with
        | End_of_file -> ":quit"
      in
      match line with
      | ":quit" -> ()
      | "" -> helper (env, st, enviroment, n)
      | _ ->
        helper
          (Haskell_lib.Interpreter.interpret_line
             line
             env
             st
             opts.dump_parsetree
             opts.print_types
             enviroment
             n)
    in
    helper (Haskell_lib.Inferencer.initial_env, 2, 0, 0))
;;
