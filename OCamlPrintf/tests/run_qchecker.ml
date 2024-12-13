(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Qchecker

let () =
  let _ : int =
    run_gen ~show_passed:true "the manual generator" TestQCheckManual.gen_structure
  in
  ()
;;

let () =
  let _ : int =
    run_gen ~show_shrinker:true "the auto generator" Ocaml_printf_lib.Ast.gen_structure
  in
  ()
;;
