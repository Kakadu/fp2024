(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Pprinter.Printer
open Parse

let print_file_with_ast file =
  Format.asprintf "Program:\n\n%s\nAST:\n\n%s" (print_file file) (Ast.show_file file)
;;

let arbitrary_file_manual =
  QCheck.make
    AstGenerator.gen_file
    ~shrink:AstShrinker.shrink_file
    ~print:print_file_with_ast
;;

let manual_test =
  QCheck.(
    Test.make ~name:"QCheck test" ~count:10 arbitrary_file_manual (fun file ->
      Result.ok file = parse parse_file (print_file file)))
;;

let () = QCheck_base_runner.run_tests_main [ manual_test ]
