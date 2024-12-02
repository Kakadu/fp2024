(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Pprint.Pprinter
open Parse.Structure
open Format
open Ast

let arbitrary_program n =
  QCheck.make (gen_program n) ~print:(asprintf "%a" pprint_program)
;;

let parse parser str = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str

let manual_test =
  QCheck.(
    Test.make (arbitrary_program 10) (fun program ->
      Result.ok program = parse parse_program (asprintf "%a\n" pprint_program program)))
;;

let () = QCheck_base_runner.run_tests_main [ manual_test ]
