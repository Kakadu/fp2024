(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Stdio
open Ocamladt_lib.Ast
open Ocamladt_lib.Parser

type config =
  { mutable dump_parsetree : bool
  ; mutable source_file : string option
  }

let read_source = function
  | Some filename -> In_channel.with_file filename ~f:In_channel.input_all
  | None -> In_channel.input_all stdin
;;

let process_input cfg =
  let code = read_source cfg.source_file in
  match parse code with
  | Ok ast ->
    if cfg.dump_parsetree
    then printf "AST: %s\n" (show_program ast)
    else printf "Code executed: %s\n" code
  | Error err -> eprintf "Error during parsing: %s\n" err
;;

let () =
  let config = { dump_parsetree = false; source_file = None } in
  let () =
    let open Arg in
    parse
      [ "-dparsetree", Unit (fun () -> config.dump_parsetree <- true), "Parse tree dump"
      ; ( "-srcfile"
        , String (fun filename -> config.source_file <- Some filename)
        , "Read code from source file" )
      ]
      (fun _ ->
        Format.eprintf "Zero arguments are not supported\n";
        exit 1)
      "REPL for Ocaml+ADT"
  in
  process_input config
;;
