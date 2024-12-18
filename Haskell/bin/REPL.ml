(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

type opts =
  { mutable dump_parsetree : bool
  ; mutable read_from_file : string
  }

let rec pp_bindinglist = function
  | [] -> Format.printf ""
  | h :: t ->
    Format.printf "Parsed: %a\n%!" Haskell_lib.Pprintast.pp_binding h;
    pp_bindinglist t
;;

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
  let text =
    match opts.read_from_file with
    | "" -> In_channel.(input_all stdin) |> String.trim
    | _ -> In_channel.with_open_text opts.read_from_file In_channel.input_all
  in
  Haskell_lib.Pai.parse_and_infer
    (String.split_on_char '\n' text)
    opts.dump_parsetree
    Haskell_lib.Inferencer.typeenv_print_int
    0
;;
