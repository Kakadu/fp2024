(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Haskell_lib.Parser

let () =
  let open Stdlib.Arg in
  parse
    [ ( "-dparsetree"
      , Unit (fun _ -> parse_and_print_line (In_channel.(input_all stdin) |> String.trim))
      , "Dump parse tree" )
    ]
    (fun _ ->
      Stdlib.Format.eprintf "Positioned arguments are not supported\n";
      Stdlib.exit 1)
    "Parse and print ast"
;;
