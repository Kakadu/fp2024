(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open AstGenerator
open Pp
open Parser
open QCheck

(* tmp *)
let gen = gen_expr (gen_block gen_stmt)
let print_file fmt = print_expr
let parse_file = Parse.parse (Expr.parse_expr Stmt.parse_block)
let arbitrary_file = QCheck.make gen ~print:(Format.asprintf "%a" print_file)

let run_tests () =
  QCheck_runner.run_tests
    [ QCheck.(
        Test.make ~count:10 arbitrary_file (fun file ->
          Result.ok file = parse_file (Format.asprintf "%a" print_file file)))
    ]
;;

let () =
  let _ = run_tests in
  ()
;;
