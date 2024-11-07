(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Qchecker

let () =
  print_endline "Testing manual generator.";
  let _ : int = TestQCheck.run_manual () in
  ()
;;
