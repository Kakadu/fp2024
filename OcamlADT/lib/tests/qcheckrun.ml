(** Copyright 2024, Rodion Suvorov, Mikhail Gavrilenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Stdio
open Ocamladt_lib.Parser
open Ocamladt_lib.Ast
open Ocamladt_lib.Pprinter
open Format
open Qshrinker

let arbitrary =
  QCheck.make
    ~print:(fun p -> Format.asprintf "%a" pp_program p)
    (* ~shrink:Shrinker.ShrinkQCheck.shrink_structure *)
    (Program.gen_program 6)
;;

let test_round_trip2 =
  QCheck.Test.make
    ~name:"round-trip parsing and pretty printing"
    ~count:2
    arbitrary
    (fun program ->
       let program_ast = show_program program in
       if String.equal program_ast "[]"
       then (
         printf "Generated empty AST. Skipping...\n";
         true)
       else (
         let printed_program = asprintf "%a" pprint_program program in
         match parse printed_program with
         | Ok parsed_program ->
           let result = List.equal Poly.equal parsed_program program in
           if result
           then printf "Success!\n"
           else
             printf
               "Mismatch! Original: %s\nParsed: %s\n"
               (show_program program)
               (show_program parsed_program);
           result
         | Error err ->
           printf "Generated program:\n%s\n\n" printed_program;
           printf "Parsing failed with error: %s\n" err;
           false))
;;

let () =
  print_endline "Testing manual generator.";
  let _ : int = QCheck_base_runner.run_tests [ test_round_trip2 ] in
  ()
;;