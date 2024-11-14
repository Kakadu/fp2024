(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Qchecker.TestQCheck

let () =
  print_endline "Testing manual generator.";
  let _ : int = run_manual () in
  ()
;;

let () =
  print_endline "Testing auto generator.";
  let _ : int = run_auto () in
  ()
;;
