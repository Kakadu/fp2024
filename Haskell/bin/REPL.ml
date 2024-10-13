(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Haskell_lib.Parser

type opts = { mutable dump_parsetree : bool }

let () =
  let opts = { dump_parsetree = false } in
  let () =
    let open Stdlib.Arg in
    parse
      [ "-dparsetree", Unit (fun _ -> opts.dump_parsetree <- true), "Dump parse tree" ]
      (fun _ ->
        Stdlib.Format.eprintf "Positioned arguments are not supported\n";
        Stdlib.exit 1)
      "Parse and print ast"
  in
  if opts.dump_parsetree
  then (
    let text = In_channel.(input_all stdin) |> String.trim in
    parse_and_print_line text)
;;
