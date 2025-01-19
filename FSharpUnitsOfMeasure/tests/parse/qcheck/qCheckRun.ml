(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Pprint.Pprinter
open Parse.Structure
open Format
open Shrinker
open Ast

let parse parser str = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str

let print_prog_with_ast prog =
  asprintf
    "AST:\n\n%s\n\nPprinted:\n\n%s\n\nParsed:\n\n%s\n\n"
    (show_program prog)
    (pprint_program prog)
    (Result.get_error (parse parse_program (pprint_program prog)))
;;

let arbitrary_gen =
  let gen = gen_program 10 in
  QCheck.make gen ~print:pprint_program
;;

let run n =
  QCheck_base_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_gen ~count:n (fun pr ->
          Ok pr <> parse parse_program (pprint_program pr)))
    ]
;;

let run_tests n =
  let _ = run n in
  ()
;;

let () =
  Arg.parse
    [ "-seed", Arg.Int QCheck_runner.set_seed, " Set seed"
    ; "-stop", Arg.Unit (fun _ -> exit 0), " Exit"
    ; "-gen", Arg.Int run_tests, " Exit"
    ]
    (fun _ -> assert false)
    "help"
;;
