(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Pprint.Pprinter
open Parse.Structure
open Format
open Ast

let parse parser str = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str

let print_prog_with_ast prog =
  asprintf
    "AST:\n\n%s\n\nPprinted:\n\n%s\n\nParsed:\n\n%s\n\n"
    (show_program prog)
    (pprint_program prog)
    (Result.get_error (parse parse_program (pprint_program prog)))
;;

let arbitrary_program n =
  let gen = gen_program n in
  QCheck.make gen ~print:print_prog_with_ast
;;

let manual_test =
  QCheck.(
    Test.make (arbitrary_program 10) (fun program ->
      let parsed = parse parse_program (pprint_program program) in
      Result.ok program = parsed))
;;

let () = QCheck_base_runner.run_tests_main [ manual_test ]
