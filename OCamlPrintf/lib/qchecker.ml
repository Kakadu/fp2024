(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let failure ast =
  Format.asprintf
    "*** PPrinter ***@.%a@.*** Ast ***@.%s@.*** Parser ***@.%s@."
    Pprinter.pp_structure
    ast
    (Ast.show_structure ast)
    (match Parser.parse (Format.asprintf "%a" Pprinter.pp_structure ast) with
     | Ok ast_parsed -> Ast.show_structure ast_parsed
     | Error error -> error)
;;

let rule_gen ?(show_passed = false) ?(show_shrinker = false) ast =
  match Parser.parse (Format.asprintf "%a" Pprinter.pp_structure ast) with
  | Ok ast_parsed ->
    if ast = ast_parsed
    then (
      if show_passed
      then Format.printf "@.*** PPrinter ***@.%a@." Pprinter.pp_structure ast;
      true)
    else (
      if show_shrinker
      then (
        Format.printf "@.*** Shrinker ***@.%a@." Pprinter.pp_structure ast;
        Format.printf "@.*** AST ***@.%s@." (Ast.show_structure ast));
      false)
  | Error _ ->
    if show_shrinker
    then Format.printf "@.*** Shrinker ***@.%a@." Pprinter.pp_structure ast;
    false
;;

let run_gen ?(show_passed = false) ?(show_shrinker = false) ?(count = 10) =
  let gen type_gen =
    QCheck.make type_gen ~print:failure ~shrink:Shrinker.shrink_structure
  in
  QCheck_base_runner.run_tests_main
    [ QCheck.Test.make
        ~count
        ~name:"the auto generator"
        (gen Ast.gen_structure)
        (fun ast -> rule_gen ~show_passed ~show_shrinker ast)
    ]
;;
