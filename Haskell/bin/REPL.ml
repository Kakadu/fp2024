(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

type opts =
  { mutable dump_parsetree : bool
  ; mutable read_from_file : string
  }

let () =
  let opts = { dump_parsetree = false; read_from_file = "" } in
  let _ =
    let open Stdlib.Arg in
    parse
      [ "-dparsetree", Unit (fun () -> opts.dump_parsetree <- true), "Dump parse tree" ]
      (fun file ->
        if Sys.file_exists file
        then opts.read_from_file <- file
        else (
          Stdlib.Format.eprintf "File doesn't exist\n";
          Stdlib.exit 1))
      "Parse and print ast"
  in
  let is_stdin =
    match opts.read_from_file with
    | "" -> true
    | _ -> false
  in
  if not is_stdin
  then
    Haskell_lib.Pai.parse_and_infer
      (String.split_on_char
         '\n'
         (In_channel.with_open_text opts.read_from_file In_channel.input_all))
      opts.dump_parsetree
      Haskell_lib.Inferencer.typeenv_print_int
      0
  else (
    let rec helper (env, st) =
      let line = input_line stdin in
      match line with
      | ":quit" -> ()
      | "" -> helper (env, st)
      | _ -> helper (Haskell_lib.Pai.parse_and_infer_line line env st)
    in
    helper (Haskell_lib.Inferencer.typeenv_empty, 0))
;;
