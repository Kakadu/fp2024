(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.REPLUtils

type opts =
  { mutable dump_parsetree : bool
  ; mutable input_file : string option
  }

let () =
  let opts = { dump_parsetree = false; input_file = None } in
  let open Stdlib.Arg in
  let speclist =
    [ ( "-dparsetree"
      , Unit (fun _ -> opts.dump_parsetree <- true)
      , "Dump parse tree, don't evaluate anything" )
    ; ( "-fromfile"
      , String (fun filename -> opts.input_file <- Some filename)
      , "Input file name" )
    ]
  in
  let anon_func _ =
    Stdlib.Format.eprintf "Positioned arguments are not supported\n";
    Stdlib.exit 1
  in
  let usage_msg = "Read-Eval-Print-Loop for F# with Active Patterns" in
  let () = parse speclist anon_func usage_msg in
  run_repl opts.dump_parsetree opts.input_file
;;
