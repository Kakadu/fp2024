(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Riscv_asm_interpreter_lib.Ast
open Riscv_asm_interpreter_lib.Parser
open Riscv_asm_interpreter_lib.Prettyprinter
open QCheck.Shrink

let shrink_ast ast = list ast

let arbitrary_ast =
  let open QCheck.Gen in
  let ast_gen = list gen_expr in
  QCheck.make
    ast_gen
    ~print:
      (Format.asprintf
         "%a"
         (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n") pp_expr))
    ~shrink:shrink_ast
;;

let run () =
  QCheck_base_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_ast (fun ast ->
          Result.ok ast
          = Angstrom.parse_string ~consume:All parse_ast (Format.asprintf "%a" pp_ast ast)))
    ]
;;

let () =
  Arg.parse
    [ "-seed", Arg.Int QCheck_base_runner.set_seed, " Set seed" ]
    print_endline
    "Testing derived generator.";
  let _ : int = run () in
  ()
;;
