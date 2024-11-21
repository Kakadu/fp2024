(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Qchecker

(* The manual generator *)
let () =
  let _ : int = run_gen TestQCheckManual.gen_structure in
  ()
;;

(* The auto generator *)
(* let () =
   let _ : int = run_gen Ocaml_printf_lib.Ast.gen_structure in
   ()
   ;; *)
