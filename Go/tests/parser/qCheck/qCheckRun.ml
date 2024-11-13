(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open AstGenerator
open Pp.Printer
open Parser
open Format

let parse_file = Parse.parse TopLevel.parse_file
let print_file fmt file = fprintf fmt "%s" (print_file file)
let arbitrary_file = QCheck.make gen_file ~print:(Format.asprintf "%a" print_file)

let run_tests () =
  QCheck_runner.run_tests
    [ QCheck.(
        Test.make ~name:"QCheck test" ~count:10 arbitrary_file (fun file ->
          Result.ok file = parse_file (Format.asprintf "%a" print_file file)))
    ]
;;

let () =
  let _ = print_endline "Running QCheck random generated tests" in
  let _ = run_tests in
  ()
;;
